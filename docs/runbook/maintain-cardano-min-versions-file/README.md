#  Cardano minimum versions for Signers

## Introduction

The [cardano-min-versions.json](../../../cardano-min-versions.json) file, which specifies the minimum supported versions of the Cardano node for Mithril signers
This ensures compatibility and functionality across different networks.

## Maintaining the file

When a new a new Cardano node version is released:
- Verify the Cardano node compatibility with Mithril nodes, especially through the CI process.
- Depending on the network:
    - For the stable networks `preview`, `preprod`, and `mainnet`: the minimum supported `cardano-node` version must be updated to be two stable releases behind the current release version.
    - For the unstable network `sanchonet`: given its current evolving nature, the minimum supported `cardano-node` version may vary from the stable one.