---
title: "Decommissioning the `testing‑sanchonet` network"
authors:
  - name: Mithril Team
tags: [cardano, sanchonet, spo, testing, network]
---

## Announcing the decommissioning of the `testing‑sanchonet` network

The `testing-sanchonet` Mithril network was introduced a year ago to allow the community to test experimental features and provide feedback to the Mithril team. In particular, it has been a valuable tool for testing the transition to the **Conway** era.

**The Mithril team sincerely appreciates the Mithril pioneer stake pool operators for participating in testing and providing valuable feedback!**

The Cardano SanchoNet network is now being sunset, having served its purpose of introducing governance features from CIP-1694 in a controlled testnet environment.

In this context, the Mithril team has decided to decommission the `testing-sanchonet` network:

- the aggregator will be terminated by **February 21, 2025**
- the network will be removed from the [Mithril explorer](https://mithril.network/explorer/?aggregator=https%3A%2F%2Faggregator.testing-sanchonet.api.mithril.network%2Faggregator)
- the repository code will be cleaned up.

If you are running a **Mithril signer on this network, you can safely terminate your node** and remove the `testing-sanchonet` configuration from your setup.

Available Mithril networks are listed on the [Network configurations](https://mithril.network/doc/manual/getting-started/network-configurations) page of the documentation website.

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
