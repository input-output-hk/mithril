---
title: Mithril Protocol’s Mainnet Beta Launch
authors:
  - name: Mithril Team
tags: [spo, mithril signer, mithril relay, mainnet, production, beta, security]
---

### Mithril relay could expose Cardano block producer internal IP when updated (Security Advisory)

The Mithril team has published a [security advisory](https://github.com/input-output-hk/mithril/security/advisories/GHSA-9m3h-72xj-x2gq) to destination of SPOs running a Mithril signer/relay on the `mainnet` infrastructure:

- **Identifier**: GHSA-9m3h-72xj-x2gq
- **Title**: Mithril relay could expose Cardano block producer internal IP when updated
- **Location**: https://github.com/input-output-hk/mithril/security/advisories/GHSA-9m3h-72xj-x2gq
- **Severity**: High (7.2/10)

:::danger

We strongly encourage all the `mainnet` SPOs to update the listening port of their **Mithril relay** in order to prevent the issue, with the process explained in the **Workarounds** section of the [security advisory](https://github.com/input-output-hk/mithril/security/advisories/GHSA-9m3h-72xj-x2gq).

:::

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
