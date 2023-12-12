# Mithril-client-wasm ![cnpm](https://img.shields.io/npm/v/mithril-client.svg) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](LICENSE-APACHE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

**This is a work in progress** 🛠 

* `mithril-client-wasm` defines all the tooling necessary to manipulate Mithril certified types available from a Mithril aggregator from a WASM compatible browser.

* The different types of available data certified by Mithril are:
    * Snapshot: list and get.
    * Mithril stake distribution: list and get.
    * Certificate: list, get, and chain validation.

## Example

Below is a basic example of how to use most of the functions exposed by the Mithril client WASM library:

```javascript
import { MithrilClient } from "@mithril-dev/mithril-client-wasm"

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

## Browser Compatiblity

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
| **Node.Js** | `15.4.0` | 2020-12-09 | - |

## Getting Help
First, check our [Developer documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-wasm-library). 

If you need more information, feel free to join IOG's Technical Community [discord server](https://discord.gg/5kaErDKDRq).

## Contributing

Thanks for considering contributing and help us on creating the Mithril protocol!

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and obviously to the
code itself.

When contributing to this project and interacting with others, please follow our [Code of Conduct](https://github.com/input-output-hk/mithril/blob/main/CODE-OF-CONDUCT.md) and our [Contributing Guidelines](https://github.com/input-output-hk/mithril/blob/main/CONTRIBUTING.md).