# Generate a `cargo nextest` filterset that excludes slow tests with unchanged dependencies

> [!NOTE]
> All paths are relative to the root of the repository.

## Introduction

This is a devbook about the [`.github/workflows/scripts/filter-slow-tests.sh`](../../../.github/workflows/scripts/filter-slow-tests.sh)
script.

This script allows generating a `cargo nextest` filterset that excludes all known slow tests unless a related source
path has been changed compared to a base branch or commit (`origin/main` by default).

## Usage

Just run the script without argument.

For example, if there's no change in the source code compared to the base branch or commit:

```shell
$ .github/workflows/scripts/filter-slow-tests.sh
not test(#*slow::*)
```

For example, if there are some changes in the source code compared to the base branch or commit:

```shell
$ .github/workflows/scripts/filter-slow-tests.sh
not test(#*slow::*) or (package(mithril-stm) and (test(#protocol::aggregate_signature::*slow::*) or test(#circuits::halo2::*slow::*)))
```

If you want to change the base branch or commit, use the `-c` or `--commit-ref` option:

```shell
$ .github/workflows/scripts/filter-slow-tests.sh -c HEAD~2
not test(#*slow::*) or (package(mithril-stm) and (test(#protocol::aggregate_signature::*slow::*)))
```

If you want to include all tests, regardless of the source code changes, use the `-a` or `--all` option:

> [!NOTE]
> This is useful if there's an external flag available that forces to run all tests (e.g., a PR label)

```shell
$ .github/workflows/scripts/filter-slow-tests.sh --all
all()
```

Full usage, together with cargo nextest:

```shell
$ cargo nextest list --workspace --profile ci -E "$(.github/workflows/scripts/filter-slow-tests.sh)"
```

## Updating the filterset

First, find the `SLOW_{RUST_PACKAGE}_TESTS` variable in the [`filter-slow-tests.sh`](../../../.github/workflows/scripts/filter-slow-tests.sh) script.

If it exists for your Rust package, update the list of slow tests with the new ones, following the format:

`path/that/trigger/tests/inclusion#rust::module::path::`

> [!NOTE]
> A file path can be used multiple times.

If it doesn't exist, first define in the `Slow tests entries`, following this template:

```shell
readonly -a SLOW_MY_PACKAGE_TESTS=(
"my-package/src/code/path#rust::module::path::"
)
```

Then integrate it in the `main` function (around other filtersets usage):

```shell
FILTERSET_MY_PACKAGE="$(generate_nextest_allowed_slow_filterset "$CHANGED_FILES" "${SLOW_MY_PACKAGE_TESTS[@]}")"
OUTPUT="$(append_output "$OUTPUT" "my-package" "$FILTERSET_MY_PACKAGE")"
```

> [!IMPORTANT]
> Make sure to test the filterset with `cargo nextest list -E "$(.github/workflows/scripts/filter-slow-tests.sh)"`
> before committing the changes.

> [!TIP]
> For your filterset to be triggered, you may have to introduce a change in the source code, such as a temporary line break.
>
> You don't even need to commit the change.
