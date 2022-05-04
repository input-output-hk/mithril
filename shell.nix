# A shell setup providing build tools and utilities for a development
# environment.
{ compiler ? "ghc8107"
  # nixpkgs 21.11
  # you can always find the latest nixpkgs revision for some edition on the corresponding git branch,
  # e.g. https://github.com/nixos/nixpkgs/tree/release-21.05 for 21.05
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz") { }
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
, # Add cardano-node & cardano-cli for our shell environment.
  # This is stable as it doesn't mix dependencies with this code-base; the
  # fetched binaries are the "standard" builds that people test. This should be
  # fast as it mostly fetches Hydra (CI) caches without building much.
  cardano-node ? import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "1.34.1";
      sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
    })
    { }
}:
let
  libs = [
    pkgs.clang
    pkgs.gtest
    pkgs.m4
    pkgs.openssl.dev
    pkgs.zlib
    pkgs.zlib.dev
    pkgs.lzma
    libsodium-vrf
  ];

  tools = [
    pkgs.rustc
    pkgs.cargo
    pkgs.pkgconfig
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.graphmod
    pkgs.haskellPackages.cabal-plan
    pkgs.haskellPackages.cabal-fmt
    # For plotting results of local-cluster benchmarks
    pkgs.gnuplot
    # For generating documentation
    pkgs.yarn
    # To interact with cardano-node and testing out things
    cardano-node.cardano-cli
    cardano-node.cardano-node
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
