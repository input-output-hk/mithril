#  Maintain the networks configuration file

## Introduction

The [networks.json](../../../networks.json) file provides essential information for configuring the Mithril nodes. In particular, it specifies the minimum supported versions of the Cardano node for Mithril signers and facilitates the verification of compatibility for the Cardano networks.

## Maintaining the file

### Assessment of compatibility:
- Update the `e2e` job in the [ci.yml](../../../.github/workflows/ci.yml) file by adding the new Cardano node version in the `cardano_node_version` variables of the matrix.
- Create a pull request to test the new Cardano node version in the devnet using CI end-to-end tests
- Assess the compatibility:
    - **Compatible without modification**: in that case Mithril nodes are compatible without modification and versions can be bumped immediately.
    - **Modifications are required by the Mithril team**: in that case the versions can be bumped after the necessary modifications are implemented and a new distribution is released.

### Update policy by network:

Here are the recommended update policies by Cardano network:
| Network | Status | Policy
|------------|------------|------------
`preview` | **stable** | the minimum supported Cardano node version should be updated to be two stable releases behind the current release version.
`preprod` | **stable** | the minimum supported Cardano node version should be updated to be two stable releases behind the current release version.
`mainnet` | **stable** | the minimum supported Cardano node version should be updated to be two stable releases behind the current release version.
`sanchonet` | **unstable** | given the unstable status of the network, the minimum supported Cardano node version may vary from the stable one.