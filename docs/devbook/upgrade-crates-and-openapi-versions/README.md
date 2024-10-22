# Upgrade the crates and openapi versions before merging a pull request

## Introduction

This devbook provides a script that allows to automatically upgrade the crates and Open API versions in the project.

Only the crates and Open API specifications with changes on the branch compared to `origin/main` are upgraded.

## Prerequisites

It requires to have `cargo-get` installed, which can be done with the following command:

```
cargo install cargo-get
```

## Usage

> [!NOTE]
> All commands are executed from the root of the repository.

### Dry-run

Just run the script without argument, by default no changes are made to the project.

```shell
. ./docs/devbook/upgrade-crates-and-openapi-versions/upgrade_crates_and_openapi_versions.sh
```

### Run

> [!IMPORTANT]
> The version bump is not based on the version on `origin/main`, but on the actual version in the branch.
>
> This means that running the script more than once will bump the versions again.

Run the script with the `--run` argument to bump the crates and openapi versions.

The script will output a preformatted commit message that can be used to create a commit when it completes.

```shell
. ./docs/devbook/upgrade-crates-and-openapi-versions/upgrade_crates_and_openapi_versions.sh --run
```

If you want the script to do the commit for you, add the `--commit` argument.

```shell
. ./docs/devbook/upgrade-crates-and-openapi-versions/upgrade_crates_and_openapi_versions.sh --run --commit
```

> [!NOTE]
> The `--commit` argument have no effect if `--run` is not specified.
