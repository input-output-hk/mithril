---
title: Mithril SanchoNet network released
authors:
  - name: Mithril Team
tags: [cardano, sanchonet, spo, testing]
---

### The release of the Mithril SanchoNet testing network

The Mithril team has released the new [`testing-sanchonet`](https://mithril.network/explorer/?aggregator=https%3A%2F%2Faggregator.testing-sanchonet.api.mithril.network%2Faggregator) Mithril network.

Mithril pioneer stake pool operators (SPOs) are welcome to run a [Mithril signer](https://mithril.network/doc/next/manual/getting-started/run-signer-node) on this network, with configuration parameters available [here](https://mithril.network/doc/manual/developer-docs/references/#mithril-networks).

It's important to note that this network will serve as the primary platform for experimenting with unstable features of the Mithril networks in the future, such as the Cardano transactions set signatures or peer-to-peer (P2P) networking.

The unstable features are consistently developed and deployed to the SanchoNet testing network directly from the `main` branch of the Mithril repository. Therefore, Mithril nodes operating on this network must be either built from the `main` branch or use pre-built binaries from the [`unstable release`](https://github.com/input-output-hk/mithril/releases/tag/unstable). Failure to update Mithril nodes to the latest unstable version or encountering a bug during development could result in operational disruptions.

Note that the `testing-sanchonet` network undergoes continuous monitoring, and its availability status is reported on this [status page](https://mithril.cronitorstatus.com/).

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
