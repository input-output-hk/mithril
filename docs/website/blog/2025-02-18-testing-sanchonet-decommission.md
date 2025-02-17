---
title: "Decommissioning the `testing‑sanchonet` network"
authors:
  - name: Mithril Team
tags: [cardano, sanchonet, spo, testing, network]
---

## Announcing the decommission of the `testing‑sanchonet` network

The `testing-sanchonet` Mithril network has been introduced a year ago to allow the community to test some experimental features and provide feedback to the Mithril team. In particular, the network has been a valuable tool for the team to test the switch to the **Conway** era.

**The Mithril team is very grateful for the Mithril pioneer SPOs' participation in testing the network and providing valuable feedback**!

The Cardano **Sanchonet** network is now being sunset as it has fulfilled its purpose of being an environment to introducing the Cardano community to governance features from **CIP-1694** in a controlled testnet environment.

In that context, the Mithril team has decided to **decommission the `testing-sanchonet` network**:

- the aggregator will be terminated by **February 21st, 2025**
- the network will be removed from the [Mithril explorer](https://mithril.network/explorer/?aggregator=https%3A%2F%2Faggregator.testing-sanchonet.api.mithril.network%2Faggregator)
- the code of the repository will be cleaned up.

If you are currently **running a Mithril signer in that network, you can now safely terminate your node** and remove the `testing-sanchonet` network configuration from your setup.

The list of available Mithril networks is listed on the [Network configurations](https://mithril.network/doc/manual/getting-started/network-configurations) page of the documentation website.

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
