---
title: Mithril client WASM breaking change
authors:
  - name: Mithril Team
tags: [mithril client, cli, breaking-change]
---

### Breaking change introduced in the unstable features of the Mithril client WASM

With the release of [distribution 2437](https://github.com/input-output-hk/mithril/releases/tag/2437.1), we introduced a breaking change to the **Mithril client WASM** version `0.4.1`. Unstable features are now activated using a **configuration option** instead of the `.unstable` property.

This change ensures a seamless transition when new `unstable` features become `stable`, eliminating breaking changes in developer code and enhancing the developer experience.

To activate `unstable` features, use the following code:

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

For questions or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
