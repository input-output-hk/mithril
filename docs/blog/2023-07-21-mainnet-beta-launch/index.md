---
title: Mithril Protocol’s Mainnet Beta Launch
authors:
- name: Mithril Team
tags: [spo, mithril signer, mainnet, production, beta]
---

### Mithril Protocol’s Mainnet Beta is launched :rocket:

The Mithril team is thrilled to announce that the **Mithril Protocol’s Mainnet Beta** is launched :tada:

Here are the next steps that we will follow in order to get the `release-mainnet` network producing its first certificates:

1. Open the first signers registrations on the `release-mainnet` network (1 epoch).
1. Create the genesis certificate of the `release-mainnet` network (1 epoch).
1. Wait for the first certificates to be produced by the `release-mainnet` network.

<!---
This diagram randomly crashes. We replace it with a screenshot until a fix is available
TODO: revert this modification when it is possible
```mermaid
timeline
section Initialization
Opening : Epoch 425 : Starts on 07/21/23 : Ends on 07/24/23 : New SPOs run a Mithril Signer on `release-mainnet` : `release-mainnet` network is opened by Mithril team
Genesis : Epoch 426 : Starts on 07/25/23 : Ends on 07/29/23 : New SPOs run a Mithril Signer on `release-mainnet` : `release-mainnet` network's certificate chain genesis operated by Mithril team
Operation : Epoch 427 : Starts on 07/30/23 : Ends on 08/03/23 : New SPOs run a Mithril Signer on `release-mainnet` : `release-mainnet` network is operational and begins producing certificates
section Observation
Monitoring : Epochs 428-432 : Starts on 08/04/23 : Ends on 08/28/23 : New SPOs run a Mithril Signer on `release-mainnet` : `release-mainnet` is monitored by Mithril Team

```
-->

[![Mainnet Beta Launch Timeline](img/mainnet-launch-timeline.png)](img/mainnet-launch-timeline.png)

This is an expected timeline, in case of modification, we will amend this post with a new one.

Also, the documentation has been updated with the configuration of the `release-mainnet` network. It is available [here](https://mithril.network/doc/manual/developer-docs/references#mithril-networks).

:::danger

As a Cardano SPO, before being able to setup a Mithril signer on the `mainnet`, it is required to complete the **step 2** of the [SPO onboarding guide](https://mithril.network/doc/manual/getting-started/SPO-on-boarding-guide#step-2-get-mithril-ready-for-mainnet)

:::

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
