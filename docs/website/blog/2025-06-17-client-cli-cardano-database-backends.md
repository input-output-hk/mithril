---
title: Switching to Cardano database incremental certification
authors:
  - name: Mithril Team
tags:
  [
    mithril client,
    library,
    cli,
    Cardano database,
    Cardano database v2,
    certification,
    backend,
  ]
---

### Switching to Cardano database incremental certification

We have recently introduced a new incremental certification process for the **Cardano node internal database**. This change is part of our ongoing efforts to enhance the Mithril protocol and improve the efficiency of Cardano node bootstrapping.

More information about the Cardano database certifications is available at:

- [Cardano node database](https://mithril.network/doc/mithril/advanced/mithril-certification/cardano-node-database)
- [Cardano node database v2](https://mithril.network/doc/mithril/advanced/mithril-certification/cardano-node-database-v2)

The **Cardano node database v2** certification is incremental and more efficient than the original **Cardano node database** certification. Thus it will progressively replace the current certification process for the Cardano node internal database.

To facilitate this transition, we have made updates to both the **Mithril client CLI** and the **Mithril client library**. These changes are designed to support the new incremental certification process and ensure a smooth experience for users.

In particular, the `cardano-db` command in the Mithril client CLI has been updated to support the new incremental certification process and eventually a seamless switch to the new backend. The command now includes a `--backend` option which allows users to specify which certification they want to use:

- `--backend v1` for the original Cardano node database certification (which is still the default version)
- `--backend v2` for the new incremental Cardano node database certification.

We intend to transition all users to the new incremental certification process over the next few months:

- [x] **Distribution [2524](https://github.com/input-output-hk/mithril/releases/tag/2524.0)**:

  - The `--backend` parameter is introduced in the `cardano-db` command
  - The backend `v1` is the default and the backend `v2` is still **unstable**
  - There is no breaking change on the client CLI
  - The `v2` backend is available thorugh the `cardano_database_v2` function of the client library

- [ ] **Distribution +1**:

  - The `v2` backend will be promoted to **stable** status and will still be optional
  - The `v1` backend will still be the default

- [ ] **Distribution +2**:

  - The `v2` backend will be promoted to **default** value
  - The `v1` backend will be deprecated

- [ ] **Distribution +3**:

  - The `v1` backend will be decommissioned and removed from the client CLI and library

- [ ] **Distribution +4**:
  - The `v1` backend will be decommissioned and removed from the signer and aggregator

If you have any questions or need assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
