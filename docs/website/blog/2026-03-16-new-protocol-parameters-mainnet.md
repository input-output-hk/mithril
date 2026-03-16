---
title: New SNARK-friendly protocol parameters applied to mainnet
authors:
  - name: Mithril Team
tags: [protocol, mainnet, spo, parameters]
---

### New SNARK-friendly protocol parameters applied to mainnet

New Mithril protocol parameters have been applied to the `release-mainnet` network at epoch `619`. These updated values are **SNARK-friendly**, meaning they are optimized for the Halo2 proof system that will be used in upcoming SNARK-based certificate verification.

#### Why the change?

The previous `k` parameter value (`2422`) was not SNARK-friendly: it required twice the number of constraints in the Halo2 circuit. A new set of parameters has been computed and validated by cryptographers and researchers to reduce this overhead while maintaining equivalent security guarantees.

#### Parameter changes

| Parameter                 | Previous value | New value |
| ------------------------- | -------------- | --------- |
| `k` (quorum)              | `2422`         | `1944`    |
| `m` (number of lotteries) | `20973`        | `16948`   |
| `phi_f` (lottery chance)  | `0.2`          | `0.2`     |

The `phi_f` parameter remains unchanged.

:::tip

More information about the protocol parameters and their impact can be found in the [Mithril documentation](https://mithril.network/doc/mithril/advanced/mithril-protocol/protocol#protocol-phases-1).

:::

:::info

No action is required from SPOs or Mithril client users. The protocol parameter update is handled by the aggregator and is transparent to signers and clients.

:::

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
