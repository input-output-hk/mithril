---
slug: 12
title: |
  12. Release process and versioning
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2026-03-20
---

## Status

Accepted

## Context

To consistently deliver software to our users, we need a release process built on a predictable versioning scheme.

### Versioning

A distribution version identifies a distribution of specific node versions and their underlying libraries.

- All software components must interoperate seamlessly with other Mithril software.
- All software components must be publishable to crates.io.
- All software components must clearly communicate their compatibility with other Mithril components to end users.

### Release process

A distribution is a software package built once and then promoted from the testing environment to the production environment. It may be signed.

- Keep it simple.
- Automate as much as possible: every step that does not require a human decision shall be automated.
- Minimize the mean time to release.

## Decision

The Mithril stack has three versioned layers:

- **HTTP API protocol**: ensures compatibility in the communication between nodes (uses Semver).
- **Crate version**: each node and library has its own version (uses Semver). The commit digest is automatically appended to the version by the CI pipeline.
- **Distribution Version**: the distribution version (uses the scheme **YYWW.patch** | **YYWW.patch-name**). The VERSION file is derived from the release tag by the pipeline.

Documentation is tied to a Distribution Version.

### Release Process

Starting immediately after a new release has been published:

1. Develop on a dedicated development branch.
1. When merging a PR into main, update the `Cargo.toml` files with the versions of the affected nodes.
1. Once merged, the CI creates an `unstable` tag and release, which is deployed to the testing environment.
1. Push a tag in the distribution version format on this commit.
1. The CI retrieves the built artifacts for this commit and generates a named pre-release, which is deployed to `pre-release-preview` for testing.
1. In the GitHub release interface, edit the newly generated release and uncheck the `This is a pre-release` checkbox.
1. The CI retrieves the built artifacts for this commit and generates a named release, which is deployed to `release-preprod` and `release-mainnet`.
1. Create a commit to promote the documentation website from future to current.

[![Release Process](./img/release_process.jpg)](./img/release_process.jpg)

### Hotfix Release

In case of a blocking issue on the release environment following a distribution release that requires an immediate fix:

1. Create a branch from the last release tag using the scheme: `hotfix/{last_distribution-version}.{last_patch_number + 1}`.
1. Develop the fix on this branch.
1. After each commit on this branch, the CI creates an `unstable` tag and release, which is not deployed to the testing environment (testing must be performed on an ad hoc environment created manually).
1. Push a tag on the branch's latest commit using the branch distribution version with a `-hotfix` suffix.
1. The CI retrieves the built artifacts for this commit and generates a named pre-release, which is deployed to `pre-release-preview` for testing.
1. In the GitHub release interface, edit the newly generated release and uncheck the `This is a pre-release` checkbox.
1. The CI retrieves the built artifacts for this commit and generates a named release, which is deployed to `release-preprod` and `release-mainnet`.
1. Merge the hotfix branch into the main branch (adapting changes as needed if they are not directly compatible with the current main branch).

### Infrastructure-only Redeployment

When a change affects only the infrastructure (e.g. a Cardano node upgrade that is compatible with the current Mithril distribution), it is possible to redeploy without creating a new distribution. See [ADR 11](/adr/11) for details.

### Synchronization with Cardano node releases

Mithril distributions are synchronized with Cardano node releases to ensure that every Mithril release ships with full support for the latest stable Cardano node version. The synchronization follows these steps:

1. **Integration build available**: a new integration version of the Cardano node is available but not yet released on GitHub.
2. **Mithril qualification**: the Mithril team qualifies the unreleased Cardano node version and prepares support for it by using the [prepare-cardano-node-artifacts](https://github.com/input-output-hk/mithril/tree/main/docs/runbook/prepare-cardano-node-artifacts) runbook.
3. **Mithril pre-release**: the Mithril team creates a new pre-release distribution that is ready to be released. It is deployed on `pre-release-preview` with the **previous** stable version of the Cardano node.
4. **Cardano node pre-release**: the Cardano node team creates a pre-release on GitHub. The release embeds Mithril nodes built from the tag of the aforementioned Mithril pre-release distribution.
5. **Joint testing**: the Cardano node and Mithril pre-release versions are tested together. The Cardano node version on `pre-release-preview` is updated to the pre-release version.
6. **Iteration**: if the Cardano node pre-release requires further minor versions before it is ready to ship, go back to step 1. A new Mithril distribution is created if necessary.
7. **Synchronous release**: the Cardano node and Mithril distributions are released simultaneously.

```mermaid
sequenceDiagram
    actor CN as Cardano Node Team
    actor MT as Mithril Team
    participant PP as pre-release-preview

    CN->>MT: Integration build available (not yet released)
    MT->>MT: Qualify integration build
    Note over MT: Prepare artifacts & Docker image<br/>(prepare-cardano-node-artifacts runbook)
    MT->>PP: Deploy Mithril pre-release<br/>(with previous stable Cardano node)
    MT->>CN: Share Mithril pre-release tag
    CN->>CN: Create Cardano node pre-release<br/>(embeds Mithril nodes from pre-release tag)
    CN->>MT: Cardano node pre-release available
    MT->>PP: Upgrade Cardano node to pre-release version
    MT->>CN: Joint testing on pre-release-preview

    alt Pre-release requires further iterations
        CN->>MT: New integration build (minor version)
        MT->>MT: Qualify & prepare new integration build
        MT->>PP: Update Mithril distribution if necessary
        CN->>CN: Create new Cardano node pre-release
        MT->>PP: Upgrade Cardano node to new pre-release version
    end

    CN->>CN: Cardano node ready for release
    CN-->>MT: Synchronous release signal
    MT->>MT: Release Mithril distribution
    CN->>CN: Release Cardano node distribution
    Note over CN,MT: Both distributions released simultaneously
```
