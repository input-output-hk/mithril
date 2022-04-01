# Mithril Test Lab

Haskell components and tests for the Mithril system.

## Building

Those build instructions use [nix](https://nixos.org) as this is the most straightforward solution to manage dependencies in the Cardano ecosystem.

Enter [nix-shell](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html):

```
nix-shell
cd mithril-test-lab
```

Build everything. _This takes a while, please be patient_

```
cabal build all
```

Run tests:

```
cabal test all
```
