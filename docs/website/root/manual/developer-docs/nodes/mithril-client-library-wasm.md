---
sidebar_position: 5
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril client library WASM

:::info

Mithril client library WASM can be used by Javascript developers to use the Mithril network in their web applications.

It is responsible for handling the different types of data certified by Mithril, and available through a Mithril aggregator:
- [**Snapshot**](../../../glossary.md#snapshot): list and get.
- [**Mithril stake distribution**](../../../glossary.md#stake-distribution): list and get.
- [**Cardano transaction**](../../../glossary.md#cardano-transaction): list & get commitments, get proofs
- [**Certificate**](../../../glossary.md#certificate): list, get, and chain validation.

:::

:::tip

* For more information about the **Mithril network**, please see the [architecture](../../../mithril/mithril-network/architecture.md) overview.

:::

:::note Mithril networks

<NetworksMatrix />

:::

## Resources

| Node | Source repository | Rust documentation |
|:-:|:-----------------:|:------------------:|
**Mithril client WASM** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client-wasm) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_client_wasm/index.html) |


## Installation

The Mithril client library is compatible with the following browsers:

| Browser | Minimum version | Released | Tested in CI |
| --- |:---:|:---:|:---:|
| **Chrome** | `54` | 2016-10-12 | :heavy_check_mark: |
| **Edge** | `79` | 2020-01-15 | - |
| **Firefox** | `38` | 2015-05-12 | :heavy_check_mark: |
| **Opera** | `41` | 2016-10-25 | - |
| **Safari** | `15.4` | 2022-03-14 | - |
| **Chrome Android** | `54` | 2016-10-19 | - |
| **Firefox for Android** | `38` | 2015-05-12 | - |
| **Opera Android** | `41` | 2016-10-25 | - |
| **Safari on iOS** | `15.4` | 2022-03-14 | - |

In your Javascript project, use `npm` to add [mithril-client-wasm](https://www.npmjs.com/package/@mithril-dev/mithril-client-wasm) library as a dependency:

```bash
npm i @mithril-dev/mithril-client-wasm
```

## Using Mithril client library

Below is a basic example of how to use most of the functions exposed by the Mithril client library:

```js
import initMithrilClient, { MithrilClient } from "@mithril-dev/mithril-client-wasm"

let aggregator_endpoint =
  "https://aggregator.testing-preview.api.mithril.network/aggregator"
let genesis_verification_key =
  "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"

const broadcast_channel = new BroadcastChannel("mithril-client");
broadcast_channel.onmessage = (e) => {
  let event = e.data;
  if (event.type == "CertificateChainValidationStarted") {
    console.log("The certificate chain validation has started");
  } else if (event.type == "CertificateValidated") {
    console.log("A certificate has been validated, certificate_hash: " + event.payload.certificate_hash);
  } else if (event.type == "CertificateChainValidated") {
    console.log("The certificate chain is valid");
  } else {
    console.log(event);
  }
};

await initMithrilClient();

let client = await new MithrilClient(
  aggregator_endpoint,
  genesis_verification_key
)
let mithril_stake_distributions_list = await client.list_mithril_stake_distributions();
console.log("stake distributions:", mithril_stake_distributions_list);

let last_mithril_stake_distribution = mithril_stake_distributions_list[0];
console.log("last_mithril_stake_distribution:", last_mithril_stake_distribution);

let last_stake_distribution = await client.get_mithril_stake_distribution(last_mithril_stake_distribution.hash);
console.log("last_stake_distribution:", last_stake_distribution);

let certificate = await client.get_mithril_certificate(last_stake_distribution.certificate_hash);
console.log("certificate:", certificate);

let last_certificate_from_chain = await client.verify_certificate_chain(certificate.hash);
console.log("verify certificate chain OK, last_certificate_from_chain:", last_certificate_from_chain);

let mithril_stake_distributions_message = await client.compute_mithril_stake_distribution_message(last_stake_distribution);
console.log("mithril_stake_distributions_message:", mithril_stake_distributions_message);

let valid_stake_distribution_message = await client.verify_message_match_certificate(mithril_stake_distributions_message, last_certificate_from_chain);
console.log("valid_stake_distribution_message:", valid_stake_distribution_message);
```

If the aggregator signs **CardanoTransactions**, you can add the code below to the previous example:

:::tip

You can verify that the aggregator signs **CardanoTransactions** by running the command below:

```bash
wget -q -O - YOUR_AGGREGATOR_ENDPOINT | jq '.capabilities.signed_entity_types | contains(["CardanoTransactions"])'
```

For example with the aggregator on `testing-sanchonet` Mithril network:

```bash
wget -q -O - https://aggregator.testing-sanchonet.api.mithril.network/aggregator | jq '.capabilities.signed_entity_types | contains(["CardanoTransactions"])'
```

:::

```js
const proof = await client.unstable.get_cardano_transaction_proofs(["CARDANO_TRANSACTION_HASH_1", "CARDANO_TRANSACTION_HASH_2"]);
console.log("Proof tx hash", proof.transactions_hashes);
console.log("Proof certificate hash", proof.certificate_hash);

let proof_certificate = await client.verify_certificate_chain(proof.certificate_hash);
console.log("verify_certificate_chain OK, last_certificate_from_chain:", proof_certificate);

let valid_cardano_transaction_proof = await client.unstable.verify_cardano_transaction_proof_then_compute_message(proof, proof_certificate);
console.log("valid_cardano_transaction_proof:", valid_cardano_transaction_proof);
```

:::tip

You can read the complete [Rust developer documentation](https://mithril.network/rust-doc/mithril_client_wasm/index.html).

:::