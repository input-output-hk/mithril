# Crate Version Checker

This script verifies that all internal workspace dependencies use the expected crate version across the repository.

## Purpose

Internal crates often evolve together and are expected to stay version-aligned.

This script helps enforce that policy by:

- reading workspace crate versions using `cargo metadata`
- inspecting all declared dependencies
- detecting version mismatches for selected internal crates
- failing when inconsistencies are found

## What it checks

For each configured crate:

- retrieve the actual workspace crate version
- inspect all workspace package dependencies
- compare dependency version requirements against the expected version

The script ignores `^` in versions, so the following versions are considered the same:

- 0.5.12
- ^0.5.12

All other differences will fail the script, for example :

- crate version `0.5.12` , dependency version `=0.5.12`
- crate version `0.5.12` , dependency version `^0.5.10`
- crate version `0.5.12` , dependency version `=0.5`
- crate version `0.5.12` , dependency version `>=0.5.12`
- crate version `0.5.12` , dependency version `~0.5.12`

You can run it from the root directory of the workspace using :
`bash ./.github/workflows/scripts/crate-version-checker/crate-version-checker.sh`

here a example of a error output:

```
Checking that all usages of mithril-common use version 0.6.72 -> KO
ERROR: mithril-aggregator-client depends to mithril-common with version ^0.6.70
ERROR: mithril-cardano-node-internal-database depends to mithril-common with version =0.6.60

...

Dependency version mismatches detected.
```

and a example of a success output:

```
Checking that all usages of mithril-common use version 0.6.72 -> OK

...

Checking that all usages of mithril-end-to-end use version 0.4.133 -> OK

All dependency versions are consistent.
```
