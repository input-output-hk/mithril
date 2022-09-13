---
title: Stake Distribution retrieval fixed
authors:
- name: Mithril Team
tags: [stake-distribution, certificate]
---

### The way the Mithril nodes retrieve the Stake Distribution is changing

**PR**: `Fix Stake Distribution retrieval` [#499](https://github.com/input-output-hk/mithril/pull/499)

**Issue**: `Stake distribution discrepancy` [#497](https://github.com/input-output-hk/mithril/issues/497)

We have noticed that the way the Mithril nodes computed the `Stake Distribution` was erroneous: the epoch that was used to make the computation was the **current epoch** instead of the **previous epoch**. This has lead to some de-synchronization between the Signers and the hosted GCP Aggregator for a few epochs.

Indeed, the `Stake Distribution` retrieved from the Cardano node depended on the time at which it was done: the nodes where having differents values that prevented them from being able to work together to produce valid multi-signatures. The problem is related to the epoch that is used (**current epoch**) to make the computation of the `Stake Distribution` when the `cardano-cli query stake-distribution` command is ran, whereas the Mithril protocol needs to work with the **previous epoch**.

A workaround is being implemented in this fix that will compute differently the `Stake Distribution` to target the **previous epoch**. To do so, the Stake value that is now retrieved sequentially for each pool available in the `cardano-cli query stake-distribution` by using the command `cardano-cli query stake-snapshot --stake-pool-id **pool-id*`. This guarantees that the `Stake Distribution` is computed deterministically on all nodes of the Mithril Network.

We will continue our efforts to enhance the way the `Stake Distribution` is retrieved in the future, and so that it works smoothly on the `mainnet` (where the numbers of pools is bigger `~3,000` vs `~100` on the `preview` network).

The SPOs need to recompile their Signer node in order to compute correctly the `Stake Distributions` on their node (as in this [guide](https://mithril.network/doc/manual/getting-started/run-signer-node)).
It should then take up to `2` epochs before they are able to successfully register their individual signatures with the Aggregator.

More information about the `Certificate Chain` and the epochs retrieval requirements is available [here](https://mithril.network/doc/mithril/mithril-protocol/certificates).

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
