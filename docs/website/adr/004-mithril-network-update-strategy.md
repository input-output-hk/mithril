---
slug: 4
title: |
  4. Mithril Network Upgrade Strategy
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2023-01-05
---

## Status

Accepted

## Context

When we will run Mithril on mainnet there will be thousands of signers running. Upgrading the version of the nodes has an impact as different versions of API, messages, signature may lead to loss of a significant part of the signers population over one epoch or more. In any case we must prevent a gap in the certificate chain while upgrading critical parts.

We need to be able to keep enough of signer nodes and the aggregator able to work together in order to produce at least one certificate per epoch.

Examples of such changes:

- change in the message structure
- change in the cryptographic algorithm
- change in communication channels

## Decision

In order to synchronize all nodes behavior transition, the Release Team will define Eras that start at a given Cardano Epoch and lasts until the next Era begins. When nodes detect an Era change, they switch from old to new behavior hence all transitioning at almost the same time.

## Consequences

### Release Team

The release team is the team responsible of releasing new versions of Mithril software. The **Release Team** will be responsible to set the Epoch at which **Eras** change using an **Era Activation Marker**. In order to be able to determine when the new Era will begin, the Release Team has to know what is the share of the total Mithril stake that can run the new behavior. Signer node software versions has to be **monitored**.

### Version monitoring

The Release Team must be aware of the software version run by the Signer nodes and their associated stake. The version is going to be added to all HTTP headers in inter-node communication. In a first step, the Aggregator nodes will record this information, and provide the mapping of stakes to Signer nodes.

This configuration works in the case where there is a centralized Aggregator Node (as it is today). In the future, there may be several Aggregator nodes working in a decentralized manner. This would mean having a separate monitoring service, and also monitor the aggregators node versions.

### Era Activation Marker

An Era Activation Marker is an information shared among all the nodes. For every upgrade, there are two phases:

- a first marker is set on the blockchain that just indicates a new Era will start soon and softwares shall be updated.
- a second marker is set that specifies the Epoch when they must switch from old to new behavior.

Every Era Activation Marker will be a transaction in the Cardano blockchain. This implies the nodes must be able to read transactions of the blockchain. Era Activation Markers can be of the same type, the first maker does not hold any Epoch information whereas the second does.

Node will check the blockchain for Markers at startup and for every new Epoch. When a node detects a Marker, it will warn the user if it does not support the incoming Era that he must upgrade his node. If the node detects it does not support the current Era, it will stop working with an explicit error message. To ease that operation, Era Activation Marker will be made sortable.

### Behavior Switch

The nodes must be able to switch from one behavior to another when the Era Epoch is reached. This means the software must embed both behaviors. The switch is developed as a one time operation, there is no rollback mechanism available. Once the Epoch is transitioned and the switch has occurred, a new software release can remove the old behavior from the codebase.

```mermaid
sequenceDiagram
    actor Release Team
    actor User
    Release Team--xChain: New Era coming soon.
    Note over Chain: new Epoch
    Old Node->>Chain: What is the latest marker?
    Chain->>Old Node: Era change soon
    New Node->>Chain: What is the last marker?
    Chain->>New Node: Era change soon
    Note over New Node: upgrade
    Loop every Epoch
        Note over Chain: new Epoch
        Old Node->>Chain: What is the last marker?
        Chain->>Old Node: Era change soon
        Old Node->>User: ⚠️ new Era incoming, please update node
        New Node->>Chain: What is the last marker?
        Chain->>New Node: Era change soon
    end
    Release Team--xChain: New Era start at Epoch XX.
    Loop every Epoch
        Note over Chain: new Epoch
        Old Node->>Chain: What is the last marker?
        Chain->>Old Node: Era change at Epoch XX
        Old Node->>User: ⚠️ new Era incoming, please update node
        New Node->>Chain: What is the last marker?
        Chain->>New Node: Era change at Epoch XX
    end
    Note over Chain: Epoch XX
    Note over Old Node,New Node: new Era
    New Node->>Chain: What is the last marker?
    Chain->>New Node: Era change at Epoch XX
    Note over New Node: switch behavior
    Old Node->>Chain: What is the last marker?
    Chain->>Old Node: Era change at Epoch XX
    Old Node->>User: 💀 unsupported Era, quit.
```
