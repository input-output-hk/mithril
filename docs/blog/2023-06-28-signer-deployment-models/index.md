---
title: Mithril Signer Deployment Models
authors:
- name: Mithril Team
tags: [spo, mithril signer, deployment model, production]
---

**Update 2023/07/07**: The Mithril Signer Deployment Models beta version is available on the `pre-release-preview` and `release-preprod` networks.

### The new Mithril Signer Deployment Models for SPOs will be introduced soon

**Epic**: `Prepare Mithril Signer deployment model for SPO` [#862](https://github.com/input-output-hk/mithril/issues/862)

:warning: The Mithril Signer Deployment Models is not deployed yet to the `pre-release-preview` and `release-preprod` networks. A special announcement will be made on the **moria** Discord channel when a new release candidate distribution is ready.

All these information will be available at the updated [`Run a Mithril Signer node (SPO)`](./manual/getting-started/run-signer-node) guide. In the mean time, a preview of the Mithril signer setup with the **production** deployment model is available [here](./manual/getting-started/run-signer-node). In the new **production** deployment model, a new **Mithril Relay** has been introduced and requires an extra setup effort versus the **naive** deployment model that is currently ran by the pioneer SPOs on the Mithril test networks.

:::info

We strongly encourage the volunteer SPOs to test the **production** deployment (once it is available of the `pre-release-preview` network) and to give us their feedback on the setup (clarity of the documentation, if you needed some fixes to make it work, ...).

:::

Here is the schema of the **production** deployment for the `mainnet`:
[![Production Mithril Signer Deployment Model](img/signer-deployment-production.jpg)](img/signer-deployment-production.jpg)

and the schema of the **naive** deployment only for the `testnet`:
[![Naive Mithril Signer Deployment Model](img/signer-deployment-naive.jpg)](img/signer-deployment-naive.jpg)

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
