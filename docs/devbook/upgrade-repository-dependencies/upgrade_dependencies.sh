#!/bin/bash

# get command line arguments to pass them to `cargo upgrade` command
# By default, we allow upgrading all dependencies to the latest one. 
# If you want to upgrade only to the compatible version, launch script with option `--compatible`
CARGO_UPGRADE_OPTIONS=${*:-"--incompatible"}

# Need to install `cargo-edit` to execute `cargo upgrade` and `cargo set-version` commands

# Update Rust dependencies
cargo update
git commit -am "chore: update Rust dependencies"

# Upgrade Rust outdated dependencies
cargo upgrade "${CARGO_UPGRADE_OPTIONS}" --verbose
cargo update
# Let the CI run the tests at the end of the script
# cargo test --all-features
git commit -am "chore: upgrade Rust dependencies"

# Bump Rust crates versions
cargo set-version --bump patch
git commit -am "chore: bump crates versions"

# Build mithril-client wasm (to have latest version used in the explorer)
make -C mithril-client-wasm build

# Upgrade the documentation website dependencies
make -C docs/website upgrade
git commit -am "chore: upgrade doc dependencies

By running 'make upgrade' command."

# Upgrade ci-test/ dependencies
make -C mithril-client-wasm upgrade-ci-test-deps
git commit -am "chore: upgrade mithril client wasm 'ci-test' dependencies

By running 'make upgrade-ci-test-deps' command."

# Upgrade client wasm examples dependencies
make -C examples/client-wasm-nodejs upgrade
make -C examples/client-wasm-web upgrade
git commit -am "chore: upgrade mithril client wasm examples dependencies

By running 'make upgrade' command in 'examples/client-wasm-nodejs' and 'examples/client-wasm-web'."

# Upgrade the explorer dependencies
pushd mithril-explorer || exit
make upgrade
popd || exit
git commit -am "chore: upgrade explorer dependencies

By running 'make upgrade' command."

# Bump Javascript packages versions

# Search all package.json files and bump the version
# and exclude `package.json` in auto-generated folders
for package_json_file in $(find . -name package.json | grep -v "/node_modules/" | grep -v "/pkg/" | grep -v "/dist/"); do
    folder="$(dirname $package_json_file)"
    pushd "$folder" || exit
    npm version patch
    popd || exit
done

make -C mithril-client-wasm ci-test-install
make -C examples/client-wasm-nodejs install
make -C examples/client-wasm-web install
make -C mithril-explorer install
make -C docs/website install

git commit -am "chore: bump javascript packages versions

By running:
- 'make install' command in 'examples/client-wasm-web'.
- 'make install' command in 'examples/client-wasm-nodejs'.
- 'make install' command in 'mithril-explorer'.
- 'make install' command in 'docs/website'."

# create a temporary script file that print "hello"
TMP_SCRIPT_DIR=/tmp/mithril
FLAKE_UPDATE_SCRIPT=nix_flake_update.sh

mkdir -p "$TMP_SCRIPT_DIR"
echo "git config --global --add safe.directory '*'
nix --extra-experimental-features 'nix-command flakes' flake update" > "$TMP_SCRIPT_DIR/$FLAKE_UPDATE_SCRIPT"

 # Upgrade Nix Flake dependencies
 docker run -v "$(pwd)":/mithril -v "$TMP_SCRIPT_DIR":/scripts/mithril -w /mithril nixos/nix /bin/sh -c ". /scripts/mithril/$FLAKE_UPDATE_SCRIPT"
 rm "$TMP_SCRIPT_DIR/$FLAKE_UPDATE_SCRIPT"

 git commit -am "chore: update nix flake dependencies

 By running 'nix flake update' command."

# Audit Rust dependencies
cargo audit
