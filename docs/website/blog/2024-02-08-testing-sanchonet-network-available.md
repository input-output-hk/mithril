---
title: Mithril Sanchonet network is released!
authors:
  - name: Mithril Team
tags: [cardano, sanchonet, spo, testing]
---

### Mithril Sanchonet testing network is released!

The Mithril team has released the new [`testing-sanchonet`](https://mithril.network/explorer/?aggregator=https%3A%2F%2Faggregator.testing-sanchonet.api.mithril.network%2Faggregator) test Mithril network.

The Mithril pioneer SPOs are welcome to run a [Mithril signer](https://mithril.network/doc/next/manual/getting-started/run-signer-node) on this network for which the configuration parameters are available [here](https://mithril.network/doc/manual/developer-docs/references/#mithril-networks).

We'd like to stress the fact that this network will be the place where we will lead experimentations of unstable features of the Mithril networks in the future (e.g. signature of the Cardano transactions set or P2P networking). 

These unstable features are continuously built and delivered to this network from the `main` branch of the Mithril repository. This means that the Mithril nodes running on this network should be built from the `main` branch, or the pre built binaries of the [`unstable release`](https://github.com/input-output-hk/mithril/releases/tag/unstable). As a consequence, the Mithril nodes might stop working properly if they are not updated to the latest unstable version or in case of a bug being part of the development process. 

However, the `testing-sanchonet` network is constantly monitored and its availability is reported on our [status page](https://mithril.cronitorstatus.com/).

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
