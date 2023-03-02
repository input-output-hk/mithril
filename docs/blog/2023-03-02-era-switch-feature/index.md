---
title: Mithril Era Switch
authors:
- name: Mithril Team
tags: [era, era activation, era markers, era switch, hard fork]
---

### An new Era Switch behavior will be introduced soon to the Mithril networks

**Epic**: `Implement eras behavior switch #707](Implement eras behavior switch` [#707](https://github.com/input-output-hk/mithril/issues/707)

:warning: The Era Switch is not deployed yet to the `pre-release-preview` and `release-preprod` network. A special announcement will be made on the **moria** Discord channel when a new release condaidate distribution is ready.

In order to guarantee that any breaking change of the Mithril nodes does not break the Certificate Chain and the that new snapshots are consistently produced, the Mithril team has developped an Era Switch Behavior. This mechanism enables to embed new features in the signer and aggregator nodes prior ro releasing them. Also the activation of these new features will take place in a coordinated manner: all the eligible nodes will hot switch to a new era at the same Cardano epoch transition. To do so, the nodes rely on a transaction that is stored on the Cardano chain that provides era markers with the associated activations epochs for the eras.

:fire: Activating this feature will require an update of configuration of the signer nodes after updating their binary:
- The `ERA_READER_ADAPTER_TYPE` env var must be set to `cardano-chain`
- The `ERA_READER_ADAPTER_PARAMS` env var must be set to the result of the command `jq -nc --arg address $(wget -q -O - **YOUR_ERA_READER_ADDRESS**) --arg verification_key $(wget -q -O - **YOUR_ERA_READER_VERIFICATION_KEY**) '{"address": $address, "verification_key": $verification_key}'` (the ****YOUR_ERA_READER_ADDRESS**** and ****YOUR_ERA_READER_VERIFICATION_KEY**** are values provided in the networks configuration matrix)

All theses information will be available at the updated [`Run a Mithril Signer node (SPO)`](https://mithril.network/doc/manual/getting-started/run-signer-node) guide.

Here is a schema that illustrates the era switch behavior:
![Era Switch Schema](./img/schema.jpg)

More information is also available at this [ADR](https://mithril.network/doc/adr/4).

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
