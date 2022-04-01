# A shell setup providing build tools and utilities for a development
# environment.
{ compiler ? "ghc8107"
  # nixpkgs 21.05 at 2021-11-17
  # you can always find the latest nixpkgs revision for some edition on the corresponding git branch,
  # e.g. https://github.com/nixos/nixpkgs/tree/release-21.05 for 21.05
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/01eaa66bb663412c31b5399334f118030a91f1aa.tar.gz") { }
, hsPkgs ? import ./default.nix { }
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
    pkgs.clang_12
    pkgs.gtest
    pkgs.go_1_17
    pkgs.gopls
    pkgs.rustup
    pkgs.m4
    pkgs.openssl.dev
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
    pkgs.postgresql
    pkgs.lsof
  ];

  haskellNixShell = hsPkgs.shellFor {
    # NOTE: Explicit list of local packages as hoogle would not work otherwise.
    # Make sure these are consistent with the packages in cabal.project.
    packages = ps: with ps; [
      mithril-monitor
      mithril-end-to-end
    ];

    # Haskell.nix managed tools (via hackage)
    tools = {
      cabal = "3.4.0.0";
      ormolu = "0.3.1.0";
      haskell-language-server = "latest";
    };

    buildInputs = libs ++ tools;

    withHoogle = true;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.llvmPackages_12.stdenv.mkDerivation {
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
  };

in
haskellNixShell // { cabalOnly = cabalShell; }
