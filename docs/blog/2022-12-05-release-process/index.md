---
title: Mithril Release Process
authors:
- name: Mithril Team
tags: [process]
---

### Mithril follows a defined release process

As the Mithril project grew and more and more SPOs became involved in testing Mithril, it became obvious we need clearer identification of artifacts running on various parts of the network. Moreover, on our road towards mainnet availability we'll need to strengthen our testing process in order to validate Mithril network on more realistic environments.

### Release Process

We want our release process to follow some basic principles:
  * _Continuous Integration_: New code is integrated into the `main` codeline frequently which triggers automated build and test process.
  * _Continuous Deployment_: New artifacts produced by the build process are continuously deployed to a suitable _environment_ where it can be used and tested by an increasing number of parties.
  * _Deployment Pipeline_: The deployment process is embodied in a _pipeline_ that describes and implements all the necessary steps to release a new version of Mithril.
  * _Artifact Promotion_: An artifact is built _once and only once_ and is _promoted_ while travelling through the build pipeline.

Here is a high-level picture of this process:

[![Release Process](./img/release_process.jpg)](./img/release_process.jpg)

* We will use a custom version based on [SemVer](https://semver.org) for all the crates, binaries and containers of the repository and for the GitHub release.
* We release a new distribution every 2 weeks (this duration is subject to changes as the project matures)
  * The released version is named after the year and its week number: **YYWW.patch** (e.g. `2250.0`).
  * In case of critical regressions happening in production, a patch version will be released in between "official" releases as a hotfix.
* A new version `YYWW.0` will have the following life cycle:
    * A commit `abc123` merged on `main` branch is deployed on the network named `testing-preview`.
    * A commit `def456` tagged with `YYWW.0-prerelease` is deployed on the network named `pre-release-preview`.
    * A GitHub release `YYWW.0-prerelease` is created and linked with the `YYWW.0-prerelease` tag and marked as `pre-release`.
    * A tag `YYWW.0-prerelease` is qualified and selected for release or rejected (and replaced by a `YYWW.1-prerelease` tag if necessary on a `fed789`).
    * If the tag `YYWW.0-prerelease` is selected, a new tag is created and name `YYWW.0` on the same commit `def456`.
    * A GitHub release `YYWW.0` is created and linked to the `YYWW.0` tag and marked as `release`.
    * The commit `def456` with tag `YYWW.0` is deployed to the network named `release-preprod`.
* The `Cargo.toml` versions of the crates are updated (if required) just before creating the `YYWW.0-prerelease` tag .
* The documentation website is also updated at the same time where the `next` version becomes the `current` version, leaving future updates be appended to the `next` version during the upcoming developments.
* In order to simplify the life of Mithril users, we have introduced a version of the `Mithril API` used between client/signer and aggregators to check if the nodes are able to communicate together (following semver and considering the versions are compatible only if they share the same minor).
* Our main distribution artefact is currently docker (OCI) images. We also provide more packages, eg. `.deb` packages or compiled binaries (some of them available on multiple platforms, e.g. Windows or macOS) to simplify users' life.
* We also publish some of our crates on the `crates.io` registry whenever a new version is created (e.g. [`mithril-stm`](https://crates.io/crates/mithril-stm)).

### Networks

* We maintain different Mithril networks (eg. servers, VMs, configurations...) to which artifacts are deployed at various stages of the process:
  * `testing-preview`: This is an internal environment based on the `preview` cardano testnet where most of the automated tests happen. It is also used to test features as soon as they are merged on the `main` branch.
  * `pre-release-preview`: This is a persistent environment based on the `preview` cardano testnet. SPOs which are active on preview are welcomed to take part in the Mithril signing process and to test new `pre-release` distributions deployed there.
  * `release-preprod`: Another persistent environment, based on the `preprod` cardano testnet, where more SPOs are expected to join and test, updated less frequently (on actual `release` distributions).
  * (_LATER_) `mainnet`: Production environment where new releases are deployed once qualifed on `release-preprod`.

### Further Reading

* The Mithril developers have redacted an ADR [Release proces and versioning](https://mithril.network/doc/adr/3/) that also describes more technically this process.
