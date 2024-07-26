# Mithril network runbook :shield:

This page gathers the available guides to operate a Mithril network.

:fire: This guides are intended to be used by expert users, and could lead to irreversible damages or loss for a network.

# Guides

| Operation                                    | Location                                                                                 | Description                                                               |
| -------------------------------------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| **Genesis manually**                         | [manual-genesis](./genesis-manually/README.md)                                           | Proceed to manual (re)genesis of the aggregator certificate chain.        |
| **Era markers**                              | [era-markers](./era-markers/README.md)                                                   | Create and update era markers on the Cardano chain.                       |
| **Downloads statistics**                     | [downloads statistics](./statistics/README.md)                                           | Display the number of downloads per day.                                  |
| **Signer registrations monitoring**          | [registrations-monitoring](./registrations-monitoring/README.md)                         | Gather aggregated data about signer registrations (versions, stake, ...). |
| **Update protocol parameters**               | [protocol-parameters](./protocol-parameters/README.md)                                   | Update the protocol parameters of a Mithril network.                      |
| **Recompute certificates hash**              | [recompute-certificates-hash](./recompute-certificates-hash/README.md)                   | Recompute the certificates hash of an aggregator.                         |
| **Fix terraform lock**                       | [terraform-lock](./terraform-lock/README.md)                                             | Fix a terraform lock in CD workflows.                                     |
| **Manage SSH access to infrastructure**      | [ssh-access](./ssh-access/README.md)                                                     | Manage SSH access on the VM of the infrastructure for a user.             |
| **Upgrade VM of infrastructure**             | [upgrade-vm](./upgrade-vm/README.md)                                                     | Upgrade the VM of the infrastructure of a Mithril network.                |
| **Create test Docker distribution**          | [test-docker-distribution](./test-docker-distribution/README.md)                         | Create a custom test Docker distribution.                                 |
| **Deploy a test network manually**           | [test-deploy-network](./test-deploy-network/README.md)                                   | Manually deploy a test distribution to a test Mithril network.            |
| **Publish crates to crates.io manually**     | [manual-publish-crates](./manual-publish-crates/README.md)                               | Manually publish Rust crates to crates.io.                                |
| **Publish packages to npm manually**         | [manual-publish-npm](./manual-publish-npm/README.md)                                     | Manually publish packages to npm registry.                                |
| **Client multi-platform test**               | [test-client-multiplatform](./test-client-multiplatform/README.md)                       | Run multi-platform client CLI binaries, docker and WASM package tests.    |
| **Maintain the networks configuration file** | [maintain-networks-configuration-file](./maintain-networks-configuration-file/README.md) | Maintain the `networks.json` file                                         |
