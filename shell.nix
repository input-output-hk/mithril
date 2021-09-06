# A shell setup providing build tools and utilities for a development
# environment. This is now based on haskell.nix and it's haskell-nix.project
# (see 'default.nix').
{ compiler ? "ghc8104"
  # nixpkgs 21.05 at 2021-07-19
, pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/4181644d09b96af0f92c2f025d3463f9d19c7790.tar.gz") { }

}:
let
  libs = [
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
