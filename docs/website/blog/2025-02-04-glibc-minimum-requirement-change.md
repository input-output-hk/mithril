---
title: Minimum required glibc version bump
authors:
  - name: Mithril Team
tags: [ci, glibc, breaking-change]
---

# Upcoming Change: Minimum required glibc version bump

:::info
This only affects users who rely on the **precompiled Linux binaries** provided by the Mithril team.

If you compile the binaries from source or use a different operating system, you are not affected by this change.
:::

Following [the deprecation of `Ubuntu 20.04` in GitHub Actions](https://github.com/actions/runner-images/issues/11101),
we are updating the minimum required glibc version for our Linux binaries from `2.31` to `2.35`.

## Current Situation

Our Continuous Integration (CI) uses GitHub Actions to build and test the Mithril binaries for different platforms.

Currently, our CI builders target `Ubuntu 20.04` to build Linux binaries, leading to a minimum required glibc version of
`2.31`.
This version is compatible with systems such as `Ubuntu 20.04` and `Debian 11 (Bullseye)`.

However, with the release of `Ubuntu 24.04`, GitHub Actions is dropping support for `Ubuntu 20.04` as they only support
the last two LTS versions.

As a result, we need to update our CI environment to use a more recent version of Ubuntu.

## Upcoming change

Distribution `2506` will be the last distribution with a minimum required glibc version of `2.31`.

After the release of distribution `2506`, our CI builders will be updated to use `Ubuntu 22.04`. Raising the minimum
required glibc version for our Linux binaries to `2.35`.

## Impact for users

The new glibc version `2.35` is compatible with systems such as `Ubuntu 22.04` and `Debian 12 (Bookworm)`.

If you are using a system with an older version of glibc, you can either:

- Upgrade your system to a newer version that supports glibc `2.35` to keep using our compiled binaries.
- Compile the binaries from source to avoid upgrading your system.

## Summary

- **Current minimum glibc version**: `2.31`
  - Examples of compatible systems: `Ubuntu 20.04`, `Debian 11 (Bullseye)`
- **New minimum glibc version**: `2.35` (effective for distributions released from **March 2025** )
  - Examples of compatible systems: `Ubuntu 22.04`, `Debian 12 (Bookworm)`

For any inquiries or assistance, don't hesitate to contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
