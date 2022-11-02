---
title: Mithril environments are updated
authors:
- name: Mithril Team
tags: [release-process, re-spin, preview, preprod, environments]
---

### The Mithril environments are updated

**PR**: `New hosted environments` [#561](https://github.com/input-output-hk/mithril/pull/561)

**Issue**: `Setup new hosted environments for testing-preview, pre-release-preview and release-preprod) with their terraform and GitHub environments` [#542](https://github.com/input-output-hk/mithril/issues/542)

On Tuesday, November 1st, 2022 the `preview` Cardano network will be re-spun and will be unavailable for 48h.

In the mean time, the Mitril team is also implementing a new Release Process that will make use of several new environments.

The Mithril testing environments are thus evolving in this context:

- The current testing environment that runs on `preview` network and that most of the Pioneer SPOs are running is **deprecated** and will be decommissioned just after the `preview` network re-spin.

- This environment will then be replaced by a new `pre-release-preview` environment open to SPOs that are eager to test pre releases of the Mithril nodes.

- A new `release-preprod` environment has been launched on the `preprod` Cardano nework and will become the `stable` environment on which SPOs are encouraged to run their nodes.

- :warning: The new `release-preprod` environment is in `unstable` status, therefore it is subject to re-genesis. We expect it to be in `stable` status within 1-2 weeks.

In the future, when Mithril reaches `mainnet`, we assume that the `release-preprod` will be replaced by a `release-mainnet` environment. This means that we will have the following environments at this time: `testing-preview`, `pre-release-preprod` and `release-mainnet`.

More information about:

- The `Mithril Networks` and their availability [here](https://mithril.network/doc/manual/developer-docs/references#mithril-networks).

- The `Release Process` is available in this [ADR](https://mithril.network/doc/adr/3).


Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
