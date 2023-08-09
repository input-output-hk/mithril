---
title: Signers list computation in Certificates
authors:
- name: Mithril Team
tags: [certificate]
---

### The way the Signers list is computed inside a Certificate on the Mithril Aggregator is changing

**PR**: `Implement filtered Signers in Certificate` [#494](https://github.com/input-output-hk/mithril/pull/494)

**Issue**: `Record 'contributing' Signers only in Certificate` [#495](https://github.com/input-output-hk/mithril/issues/495)

Before this change, the list of Signers displayed in the `Certificate` detail of the [Mithril Explorer](https://mithril.network/explorer/) was the list of **all eligible** Signers of the epoch used for signing (those who have successfully registered with the Mithril Aggregator `2` epochs earlier).

Now that this change has been merged, the list of Signers displayed will only include the **contributing** Signers, which means only those who have successfully sent individual signatures.

Note that the already existing `Certificates` will not be updated as this would break the `Certificate Chain` and therefore would involve the bootstraping of a new `Genesis Certificate`.

This change is transparent to the Signer nodes runned by the SPOs and does not require any specific action from them.

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
