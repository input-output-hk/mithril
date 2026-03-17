---
title: Mainnet protocol parameters updated to support SNARK verification
authors:
  - name: Mithril Team
tags: [protocol, mainnet, spo, parameters]
---

### Optimizing Mithril protocol parameters for SNARK-based verification

New Mithril protocol parameters were applied to the `release-mainnet` network at epoch `619`. These updated values are **SNARK-friendly**, meaning they are optimized for the Halo2 proof system used in upcoming SNARK-based certificate verification.

#### Why the change?

This update is essential for the upcoming use of SNARK-based certificate verification in Mithril. The previous `k` parameter value (`2422`) required twice the number of constraints in the Halo2 circuit, increasing the cost of proof generation. To address this, the team of researchers and cryptographers computed and validated a new set of parameters to reduce this overhead while maintaining equivalent security guarantees.

#### Parameter changes

| Parameter                 | Previous value | New value |
| ------------------------- | -------------- | --------- |
| `k` (quorum)              | `2422`         | `1944`    |
| `m` (number of lotteries) | `20973`        | `16948`   |
| `phi_f` (lottery chance)  | `0.2`          | `0.2`     |

The `phi_f` parameter remains unchanged.

:::tip

You can find more information about protocol parameters and their impact in the [Mithril documentation](https://mithril.network/doc/mithril/advanced/mithril-protocol/protocol#protocol-phases-1).

:::

:::info

No action is required from stake pool operators or Mithril client users. The protocol parameter update is handled by the aggregator and is transparent to signers and clients.

:::

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
