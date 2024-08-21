# Upgrade the project's dependencies in the repository

## Introduction

This runbook provides step-by-step instructions to upgrade the dependencies in the repository, including Rust crates, documentation, and JavaScript packages.

## Steps

### Update Rust dependencies

From the root of the repository, run:

```bash
cargo update
```

Create a dedicated commit, e.g.:

```bash
chore: update Rust dependencies

By running 'cargo update' command.
```

### Bump Rust crates versions

Increment the patch versions in the `Cargo.toml` by 1 (eg. from `x.x.1` to `x.x.2`) for each Rust crate in the repository.

Create a dedicated commit, e.g:

```bash
chore: bump crates versions
```

### Upgrade the documentation website dependencies

From the root of the repository, run:

```bash
cd docs/website
make upgrade
```

Create a dedicated commit, e.g.:

```bash
chore: upgrade doc dependencies

By running 'make upgrade' command.
```

### Upgrade the explorer dependencies

From the root of the repository, run:

```bash
cd mithril-explorer
make upgrade
```

Create a dedicated commit, e.g.:

```bash
chore: upgrade explorer dependencies

By running 'make upgrade' command.
```

### Upgrade `www/` and `www-test/` dependencies

From the root of the repository, run:

```bash
cd mithril-client-wasm
make upgrade-www-deps
```

Create a dedicated commit, e.g.:

```bash
chore: upgrade mithril client wasm 'www' and 'www-test' dependencies

By running 'make upgrade-www-deps' command.
```

### Bump Javascript packages versions

Increment the patch versions in the `package.json` by 1 (eg. from `x.x.1` to `x.x.2`) for each Javascript package in the repository (`www`, `www-test` and `mithril-explorer`, `docs/website`).

Then, from the root of the repository, run the commands:

```bash
cd mithril-client-wasm
make www-install && make www-test-install
```

```bash
cd mithril-explorer
make install
```

```bash
cd docs/website
make install
```

Create a dedicated commit, e.g.:

```bash
chore: bump mithril client wasm 'www' and 'www-test' dependencies

By running:
- 'make www-install' command in 'mithril-client-wasm'.
- 'make www-test-install' command in 'mithril-client-wasm'.
- 'make install' command in 'mithril-explorer'.
- 'make install' command in 'docs/website'.
```

### Upgrade Nix Flake dependencies

```bash
nix flake update
```

Create a dedicated commit, e.g.:

```bash
chore: update nix flake dependencies

By running 'nix flake update' command.
```
