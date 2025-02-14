---
title: Minimum required `glibc` version bump
authors:
  - name: Mithril Team
tags: [ci, glibc, breaking-change]
---

# Upcoming change: minimum required `glibc` version bump

:::info

- This change **only affects users who rely on the precompiled Linux binaries** provided by the Mithril team.
- If you compile the binaries from source or use a different operating system, you are **not affected**.
  :::

## Background

Our **continuous integration (CI)** system uses GitHub Actions to build and test Mithril binaries across different platforms.

Currently, our CI targets `Ubuntu 20.04`, which results in a minimum required `glibc` version `2.31`. This version is compatible with:

- `Ubuntu 20.04`
- `Debian 11 (Bullseye)`.

However, **GitHub Actions is deprecating Ubuntu 20.04** following the release of `Ubuntu 24.04`. Since GitHub Actions only supports the last two (LTS) versions, we need to update our CI environment to use a more recent version of Ubuntu.

## Upcoming changes

- Distribution `2506` will be the last release with a minimum required `glibc version 2.31`
- After distribution `2506`, our CI builds will be updated to Ubuntu `22.04`, raising the minimum required `glibc` version for our Linux binaries to `2.35`.

## Impact for users

The new `glibc 2.35` version is compatible with:

- Ubuntu `22.04`
- `Debian 12 (Bookworm)`.

If your system uses an **older `glibc` version**, you have two options:

1. **Upgrade your system** to a version that supports glibc `2.35`
2. Compile the binaries from source.

## Summary

- **Current minimum `glibc` version**: `2.31`
  - Compatible with `Ubuntu 20.04`, `Debian 11 (Bullseye)`
- **New minimum `glibc` version**: **`2.35`** (effective for distributions released from **March 2025**)
  - Compatible with `Ubuntu 22.04`, `Debian 12 (Bookworm)`.

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
