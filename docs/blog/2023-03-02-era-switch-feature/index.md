---
title: Mithril Era Switch
authors:
- name: Mithril Team
tags: [era, era activation, era markers, era switch, hard fork]
---

**Update 2023/03/10**: The Era Switch behavior has been activated on the `release-preprod` network.

**Update 2023/03/08**: The Era Switch behavior has been activated on the `pre-release-preview` network.

### An new Era Switch behavior will be introduced soon to the Mithril networks

**Epic**: `Implement eras behavior switch #707](Implement eras behavior switch` [#707](https://github.com/input-output-hk/mithril/issues/707)

:warning: The Era Switch is not deployed yet to the `pre-release-preview` and `release-preprod` network. A special announcement will be made on the **moria** Discord channel when a new release candidate distribution is ready.

In order to guarantee that any breaking change of the Mithril nodes does not break the Certificate Chain and the that new snapshots are consistently produced, the Mithril team has developped an Era Switch Behavior. This mechanism enables to embed new features in the signer and aggregator nodes prior ro releasing them. Also the activation of these new features will take place in a coordinated manner: all the eligible nodes will hot switch to a new era at the same Cardano epoch transition. To do so, the nodes rely on a transaction that is stored on the Cardano chain that provides era markers with the associated activations epochs for the eras.

:fire: Activating this feature will require an update of configuration of the signer nodes after updating their binary:
- The `ERA_READER_ADAPTER_TYPE` env var must be set to `cardano-chain`
- The `ERA_READER_ADAPTER_PARAMS` env var must be set to the result of the command `jq -nc --arg address $(wget -q -O - **YOUR_ERA_READER_ADDRESS**) --arg verification_key $(wget -q -O - **YOUR_ERA_READER_VERIFICATION_KEY**) '{"address": $address, "verification_key": $verification_key}'` (the ****YOUR_ERA_READER_ADDRESS**** and ****YOUR_ERA_READER_VERIFICATION_KEY**** are values provided in the networks configuration matrix)

Here is the configuration values that should be used on `pre-release-preview`:
```bash
ERA_READER_ADAPTER_TYPE=cardano-chain
ERA_READER_ADAPTER_PARAMS={"address":"addr_test1qrv5xfwh043mlc3vk5d97s4nmhxu7cmleyssvhx37gkfyejfe8d38v3vsfgetjafgrsdc49krug8wf04h5rmtengtejqlxrksk","verification_key":"5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d"}
```

Here is the configuration values that should be used on `release-preprod`:
```bash
ERA_READER_ADAPTER_TYPE=cardano-chain
ERA_READER_ADAPTER_PARAMS={"address":"addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx","verification_key":"5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d"}
```

All theses information will be available at the updated [`Run a Mithril Signer node (SPO)`](https://mithril.network/doc/manual/getting-started/run-signer-node) guide.

Here is a schema that illustrates the era switch behavior:
[![Era Switch Schema](./img/schema.jpg)](./img/schema.jpg)

More information is also available at this [ADR](https://mithril.network/doc/adr/4).

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
