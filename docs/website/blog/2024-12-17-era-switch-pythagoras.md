---
title: Era switch to Pythagoras
authors:
  - name: Mithril Team
tags: [era, switch, thales, pythagoras]
---

### Era switch to Pythagoras

We have introduced the **Pythagoras era** in the Mithril networks. The era switch to `Pythagoras` is a significant milestone that brings new features and security improvements to the Mithril protocol.

:::danger

The **Mithril signer versions** compatible with the new `Pythagoras` era are:

- `0.2.221`
- `0.2.209`
- `0.2.200`

All the other versions are not compatible with the new era and must be updated.

:::

:::tip

You can easily update your Mithril signer with this one line command (it will be downloaded in the current directory, a custom folder can be specified with `-p` option):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d latest -p $(pwd)
```

:::

The era switch to `Pythagoras` plan is the following:

- [x] Era switch on `pre-release-preview`:
  - [x] Create the era switch transaction (done at epoch `757`).
  - [x] Era switch to `Pythagoras` took place at the transition to epoch `759`.
- [x] Era switch on `release-preprod`:
  - [x] Create the era switch transaction (done at epoch `184`).
  - [x] Era switch to `Pythagoras` took place at the transition to epoch `186`.
- [ ] Era switch on `release-mainnet`:
  - [ ] Create the era switch transaction (**to be done, expected early January 2025**).
  - [ ] Era switch to `Pythagoras`.

:::info

We use the **era switch mechanism to introduce breaking changes** in the Mithril protocol. The new features and improvements are not backward compatible with the previous era. This means that a large majority of at least `95%` of the stake running the new version is required to activate the new era. More information about the era switch mechanism can be found in the [Mithril Network Upgrade Strategy](https://mithril.network/doc/adr/4) ADR.

:::

For any inquiries or assistance, don't hesitate to contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
