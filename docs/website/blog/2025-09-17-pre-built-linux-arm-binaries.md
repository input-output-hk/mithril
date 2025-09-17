---
title: Pre-built Linux ARM binaries are now available
authors:
  - name: Mithril Team
tags: [release, distribution, 2537, arm, linux, pre-built, binaries]
---

### Pre-built Linux ARM binaries are now available

With the release of the [`2537.0`](https://github.com/input-output-hk/mithril/releases/tag/2537.0) distribution, **Linux ARM pre-built binaries** are now available for the Mithril nodes:

- Download the binaries directly (in a file attached to the GitHub release with the suffix `-linux-arm64.tar.gz`)
- Download the Debian package (in a file attached to the GitHub release with the suffix `-arm64.deb`)
- Or use the [one-liner installer](https://mithril.network/doc/dev-blog/2024/11/25/one-line-binaries-installer) to install them.

To provide a clear overview of the supported platforms, we have added a new "Platform Support" section in the release notes, which is reproduced below for your convenience (and might change in the future).

| Binary             | Linux x64 | Linux arm64 | macOS arm64 | Windows x64 |
| ------------------ | :-------: | :---------: | :---------: | :---------: |
| mithril-aggregator |    ✔     |   ✔ ⁽\*⁾   |     ⛔      |     ⛔      |
| mithril-signer     |    ✔     |   ✔ ⁽\*⁾   |     ⛔      |     ⛔      |
| mithril-client     |    ✔     |   ✔ ⁽\*⁾   |     ✔      |     ✔      |

⁽\*⁾⚠️ Linux arm64 builds are not guaranteed, use at your own risk.

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
