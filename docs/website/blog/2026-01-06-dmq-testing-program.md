---
title: DMQ testing program with SPOs
authors:
  - name: Mithril Team
tags: [DMQ, decentralization, testing program]
---

### DMQ testing program with SPOs

We are excited to announce the launch of a testing program for the **Decentralized Message Queue (DMQ)** feature with a group of Stake Pool Operators (SPOs). This program will take place on the `pre-release-preview` network and aims to gather feedback, identify potential issues, and ensure the robustness of the DMQ implementation before its wider release.

#### What is DMQ?

The DMQ protocol, specified in [CIP-0137](https://cips.cardano.org/cip/CIP-0137), is a major step forward for the Mithril network. It enables **multiple aggregators** to operate simultaneously on the same Mithril network, significantly improving **decentralization** and **resilience**. This enhancement reduces single points of failure and strengthens the overall robustness of the certificate production process.

#### Call for SPO participation

We invite interested SPOs to join the testing program! The only requirement is to run a Mithril signer on the `pre-release-preview` network. If you want to participate, please reach out to us on the Mithril Discord channel.

#### Important note on adoption

For the DMQ protocol to function effectively, a **minimum adoption of 60% of the stake** among SPOs is required. During the ramp-up phase, the legacy signature registration mechanism will remain active to ensure continuity and avoid any disruption. Once the vast majority of SPOs have adopted DMQ, the legacy mechanism will be deactivated.

:::tip

The Mithril team will provide dedicated support and guidance to help SPOs set up and operate their DMQ nodes.

:::

#### Rollout plan

The roll-out plan of the DMQ network is the following:

- [ ] **Distribution +1**: `unstable` on the `pre-release-preview` network
  - [ ] Activation of DMQ on the `pre-release-preview` network
  - [ ] Release of a guide for setting up a DMQ node in the SPO infrastructure
  - [ ] 4-6 weeks testing program with selected SPOs
- [ ] **Distribution +2**: `stable` on the `release-preprod` and `release-mainnet` networks with fallback
  - [ ] Activation of DMQ on the `release-preprod` and `release-mainnet` networks
  - [ ] Activation of the fallback to the legacy signature registration mechanism for all SPOs
  - [ ] Release of an updated guide for setting up a DMQ node in the SPO infrastructure
  - [ ] Monitoring and support for SPOs running DMQ nodes
- [ ] **Distribution +3**: `stable` on the `release-preprod` and `release-mainnet` networks
  - [ ] Deactivation of the fallback to the legacy signature registration mechanism for all SPOs
  - [ ] Monitoring and support for SPOs running DMQ nodes.

For any inquiries or assistance, don't hesitate to contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
