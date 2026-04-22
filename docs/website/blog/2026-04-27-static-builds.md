---
title: Statically built Mithril binaries
authors:
  - name: Mithril Team
tags: [release, distribution, 2617, static, musl, binaries, ci, docker]
---

### Statically built Mithril binaries

With the [`2617.0`](https://github.com/input-output-hk/mithril/releases/tag/2617.0) distribution, all Mithril nodes are now statically built in the CI pipelines. This significantly simplifies deployment and removes a long-standing portability constraint.

#### What has changed?

- The aggregator, signer, client, and relay are statically built on both the GitHub and Hydra CI pipelines using the `musl` target
- Default feature updates:
  - `rustls` is now the default TLS backend for all Mithril nodes, replacing the system TLS library
  - `num-integer-backend` is now the default feature in the `mithril-stm` library
- Default memory allocator updates:
  - Jemallocator is now a default dependency on Linux x86_64, replacing the previous optional feature
  - Mimalloc is now a default dependency on Linux ARM64
- Docker base images have been upgraded to `debian:13-slim`
- `glibc` is no longer required, as binaries are now statically linked.

#### Why does it matter?

Previously, the pre-built Linux binaries depended on a minimum version of the `glibc` shared library (see the [previous announcement](https://mithril.network/doc/dev-blog/2025/02/04/glibc-minimum-requirement-change)). This meant that operators running older Linux distributions had to either upgrade their system or build the binaries from source.

Static builds solve this class of issues by bundling all dependencies into a single self-contained binary:

- **Wider compatibility**: the binaries now run on almost any modern Linux distribution regardless of the installed `glibc` version
- **Simpler deployment**: no more system-level prerequisites beyond the Linux kernel itself
- **Smaller Docker images**: combined with the upgrade to `debian:13-slim`, Mithril Docker images are leaner and faster to pull
- **Better determinism**: builds are more reproducible across environments (developer machines, CI runners).

#### How to get the static binaries?

The static binaries are available from the usual channels starting with distribution `2617.0`:

- The [GitHub release](https://github.com/input-output-hk/mithril/releases/tag/2617.0) assets (tarballs and Debian packages)
- The [one-line installer](https://mithril.network/doc/dev-blog/2024/11/25/one-line-binaries-installer)
- [Docker images](https://github.com/orgs/input-output-hk/packages?repo_name=mithril) on the GitHub Container Registry.

:::info

No action is required: the binaries keep the same names, CLI, and configuration options. The change is transparent to operators and users.

:::

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
