---
title: Mithril client WASM breaking change
authors:
  - name: Mithril Team
tags: [mithril client, cli, breaking-change]
---

### Breaking change introduced in the unstable features of the Mithril client WASM

With the release of the new distribution [2437](https://github.com/input-output-hk/mithril/releases/tag/2437.1), we have introduced a breaking change to the **Mithril client WASM** version `0.4.1`: the activation of the unstable features is now done with a **configuration option of the client** instead of using the special `.unstable` property of the client.

This means that when a new `unstable` feature is switched to `stable`, there will be no breaking change in the developers code using the Mithril client WASM, thus providing a seamless transition and a better developer experience.

Here is the code used to activate the `unstable` features with the client options:

```js
let client = new MithrilClient(aggregator_endpoint, genesis_verification_key, {
  // The following option activates the unstable features of the client.
  // Unstable features will trigger an error if this option is not set.
  unstable: true,
});
```

The previous `client.unstable` implementation is not supported anymore and must be replaced with `client`:

```js
// Before
let mithril_stake_distributions_message =
  await client.unstable.compute_mithril_stake_distribution_message(
    last_stake_distribution,
  );
```

```js
// After
let mithril_stake_distributions_message =
  await client.compute_mithril_stake_distribution_message(
    last_stake_distribution,
  );
```

The Mithril client WASM documentation is available [here](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library-wasm).

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
