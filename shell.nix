# A shell setup providing build tools and utilities for a development
# environment.
{ compiler ? "ghc8107"
  # nixpkgs 21.05 at 2021-11-17
  # you can always find the latest nixpkgs revision for some edition on the corresponding git branch,
  # e.g. https://github.com/nixos/nixpkgs/tree/release-21.05 for 21.05
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/01eaa66bb663412c31b5399334f118030a91f1aa.tar.gz") { }

}:
let
  libs = [
    pkgs.gtest
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

    # Make the shell suitable for the stack nix integration
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
  };

in
cabalShell
