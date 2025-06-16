---
title: Introducing the UTXO snapshot converter command in the Mithril client CLI
authors:
  - name: Mithril Team
tags: [mithril client, cli, command, utxo-hd, conversion]
---

### Introducing the UTXO snapshot converter command in the Mithril client CLI

With the release of the new [2524](https://github.com/input-output-hk/mithril/releases/tag/2524.0) distribution, the **tools** command has been added to the **Mithril client CLI**.

Since the release of Cardano node [v.10.4.1](https://github.com/IntersectMBO/cardano-node/releases/tag/10.4.1), the Mithril aggregator node has been producing snapshots of the Cardano database using the `InMemory` UTXO-HD flavor.

When restoring a Cardano database snapshot, if the Cardano node is configured to use the on-disk (LMDB) or `Legacy` format (as in Cardano node v.10.3 and earlier), the snapshot must be converted before restarting the node.

The new `utxo-hd snapshot-converter` subcommand simplifies node bootstrapping by converting the restored ledger state snapshot into the required format (`LMDB` or `Legacy`).

This conversion tool is currently marked as `unstable` and is available on Linux, macOS, and Windows.

It operates using the `snapshot-converter` binary included with the Cardano node distribution.

Usage:

```bash
mithril-client --unstable tools utxo-hd snapshot-converter --db-directory $DB_DIRECTORY --cardano-node-version 10.1.4 --utxo-hd-flavor $UTXO_HD_FLAVOR --cardano-network $CARDANO_NETWORK
```

Parameters:

- `--db-directory`: path to the Cardano database directory
- `--cardano-node-version`: version used to download the `snapshot-converter` binary (specific version, eg, `10.1.4`, `latest`, or `prerelease`)
- `--utxo-hd-flavor`: target UTXO-HD flavor (`LMDB` or `Legacy`)
- `--cardano-network`: Cardano network (`preview`, `preprod` or `mainnet`).

The 'Bootstrap a Cardano node' guide now includes a new optional [step](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node#step-5-optional-convert-the-ledger-state-snapshot-to-another-flavor) that describes how to use the command.

The documentation for the **client CLI** has been updated accordingly and is available [here](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client#tools-unstable).

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
