---
title: Pre-built Linux ARM binaries are now available
authors:
  - name: Mithril Team
tags: [release, distribution, 2537, arm, linux, pre-built, binaries]
---

### Pre-built Linux ARM binaries are now available

With the release of the [`2537.0`](https://github.com/input-output-hk/mithril/releases/tag/2537.0) distribution, **pre-built Linux ARM binaries** are now available for Mithril nodes:

- Download the binaries directly (files attached to the GitHub release with the suffix `-linux-arm64.tar.gz`)
- Download the Debian package (files attached to the GitHub release with the suffix `-arm64.deb`)
- Use the [one-liner installer](https://mithril.network/doc/dev-blog/2024/11/25/one-line-binaries-installer) to install them.

To provide a clear overview of supported platforms, the team has added a new 'Platform support' section in the release notes, reproduced below for convenience (note that it may change in the future).

| Binary             | Linux x64 | Linux arm64 | macOS arm64 | Windows x64 |
| ------------------ | :-------: | :---------: | :---------: | :---------: |
| mithril-aggregator |    ✔     |   ✔ ⁽\*⁾   |     ⛔      |     ⛔      |
| mithril-signer     |    ✔     |   ✔ ⁽\*⁾   |     ⛔      |     ⛔      |
| mithril-client     |    ✔     |   ✔ ⁽\*⁾   |     ✔      |     ✔      |

⁽\*⁾⚠️ Linux arm64 builds are provided on a best-effort basis and are not officially supported.

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
