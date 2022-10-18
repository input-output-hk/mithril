---
slug: 3
title: |
  3. Release process and versioning
authors: [Jean-Philippe Raynaud, Denis Jouenne, Grégoire Hubert]
tags: [Draft]
---

## Status

**draft**

## Context

In order to deliver regularly the software to our users, we should implement a release process based on a predictable versioning scheme. 

### Versioning

A Release Version determines a distribution of determined node versions and underlying libraries.

 * our softwares must be able to interact seamlessly with other Mithril software
 * our softwares must be able to be hosted on crates.io
 * must clearly indicate compatibility with other Mithril components to end users
 

### Release process

A Release is a software package that is built once and then promoted from the testing environment to the production environment. It can be signed.

 * Keep it simple
 * Automated as much as possible: all points not requiring human decision shall be automated
 * Minimize the mean time to release 

## Decision

There are 3 versioned layers in the Mithril stack:

 * HTTP API protocol to ensure compatibility in the communication between nodes (use Semver)
 * Crate version: each node & library has its own version (use Semver) the commit digest is automatically added to the version by the CI pipeline.
 * Release Version: the distribution version (use version scheme YYWW.patch|YYWW.patch-name). The VERSION file is computed by the pipeline from the tag release.

The documentation is tied to a Release Version.

### Release Process

Starting just after a new release has been made:

1. Develop on a dedicated development branch
1. When merging PR on main: update the `Cargo.toml` files with version of the updated nodes
1. Once merged, the CI creates an `unstable` tag & release which is deployed on testing environment
1. Push a tag using the distribution version format on this commit with a `-pre_release` suffix.
1. The CI gets the built artifacts associated with this commit and generates a named pre-release which is deployed on `pre-release` for testing.
1. Push a tag using the distribution version format on this commit without the `-pre_release` suffix.
1. The CI gets the built artifacts associated with this commit and generates a named release which is deployed on `pre-release` for testing.
1. In the release GitHub interface, edit the newly generated release, uncheck the `This is a pre-release` tick box.
1. The CI gets the built artifacts associated with this commit and generates a named release which is deployed on `release`.
1. Update the documentation website from future to current.

