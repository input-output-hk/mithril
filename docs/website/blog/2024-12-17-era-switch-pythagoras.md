---
title: Era switch to Pythagoras
authors:
  - name: Mithril Team
tags: [era, switch, thales, pythagoras]
---

### Era switch to Pythagoras

We have introduced the **Pythagoras era** in the Mithril networks. The switch to `Pythagoras` is a significant milestone that brings new features and improvements to the Mithril protocol.

:::danger

**Mithril signer versions** compatible with the new `Pythagoras` era are:

- `0.2.221`
- `0.2.209`
- `0.2.200`

All other versions are **not** compatible with the new era and must be updated.

:::

:::tip

You can easily update your Mithril signer with this one-line command (it will be downloaded to the current directory by default; you can specify a custom folder with the `-p` option):


```bash
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d latest -p $(pwd)
```

:::

#### Era switch plan for `Pythagoras`

- **Pre-release-preview** network  
  - [x] Create the era switch transaction (done at epoch `757`).  
  - [x] Complete the era switch to `Pythagoras` at the transition to epoch `759`.

- **Release-preprod** network  
  - [x] Create the era switch transaction (done at epoch `184`).  
  - [x] Complete the era switch to `Pythagoras` at the transition to epoch `186`.

- **Release-mainnet** network  
  - [ ] Create the era switch transaction (planned for early January 2025).  
  - [ ] Complete the era switch to `Pythagoras`.

:::info
We use the **era switch mechanism** to introduce breaking changes in the Mithril protocol. Because these features are not backward compatible with the previous era, at least **95% of the stake** must be running the new version for `Pythagoras` to activate. Refer to the [Mithril Network Upgrade Strategy](https://mithril.network/doc/adr/4) ADR for more details.
:::

If you have any questions or need assistance, feel free to contact the team on our [Discord channel](https://discord.gg/5kaErDKDRq).


For any inquiries or assistance, don't hesitate to contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
