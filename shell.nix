# A shell setup providing build tools and utilities for a development
# environment.
{ compiler ? "ghc8107"
  # nixpkgs 21.05 at 2021-11-17
  # you can always find the latest nixpkgs revision for some edition on the corresponding git branch,
  # e.g. https://github.com/nixos/nixpkgs/tree/release-21.05 for 21.05
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/01eaa66bb663412c31b5399334f118030a91f1aa.tar.gz") { }
, libsodium-vrf ? pkgs.libsodium.overrideAttrs (oldAttrs: {
    name = "libsodium-1.0.18-vrf";
    src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "libsodium";
      # branch tdammers/rebased-vrf
      rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
      sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
    };
    nativeBuildInputs = [ pkgs.autoreconfHook ];
    configureFlags = "--enable-static";
  })
}:
let
  libs = [
    pkgs.clang
    pkgs.gtest
    pkgs.rustup
    pkgs.m4
    pkgs.openssl.dev
    pkgs.zlib
    pkgs.zlib.dev
    pkgs.lzma
    libsodium-vrf
  ];

  tools = [
    pkgs.pkgconfig
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.graphmod
    pkgs.haskellPackages.cabal-plan
    pkgs.haskellPackages.cabal-fmt
    # For plotting results of local-cluster benchmarks
    pkgs.gnuplot
  ];

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "mithril-stack-shell";

    buildInputs = tools ++ libs ++ [
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.stack
      pkgs.haskell-language-server
      pkgs.ormolu
      pkgs.git
      pkgs.pkgconfig
    ];

    # Ensure that libz.so and other libraries are available to TH splices.
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";
    # This is required by postgres
    LC_ALL = "C.UTF-8";

    # Make the shell suitable for the stack nix integration
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
    phases = [ "buildPhase" ];

    buildPhase = ''
    echo "------------------------------------------------------------" >>$out
    echo " WARNING: the existence of this path is not guaranteed." >>$out
    echo " It is an internal implementation detail for pkgs.mkShell."   >>$out
    echo "------------------------------------------------------------" >>$out
    echo >> $out
    # Record all build inputs as runtime dependencies
    export >> $out
  '';
  };

in
cabalShell
