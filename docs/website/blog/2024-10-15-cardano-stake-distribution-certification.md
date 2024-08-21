---
title: Certification of Cardano stake distribution
authors:
  - name: Mithril Team
tags: [certification, cardano stake distribution]
---

### Certification of Cardano stake distribution

We have introduced the certification of **Cardano stake distribution** in the Mithril networks with the release of the new distribution [2442](https://github.com/input-output-hk/mithril/releases/tag/2442.0).

This update leverages the Mithril network to provide certified Cardano stake distribution data without requiring a full Cardano node, which is particularly useful for applications such as bridges, sidechains, and layer 2 solutions.

Key changes include:

- Certification of Cardano stake distribution at each epoch transition.
- New HTTP routes in the Aggregator REST API to access this certified data.
- Updates to the Mithril client library and CLI for retrieving and verifying Cardano stake distribution.
- WASM client support for these functionalities.
- Mithril Explorer now displays certified Cardano stake distribution.

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
