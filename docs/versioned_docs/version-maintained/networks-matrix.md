---
unlisted: true
hide_title: true
hide_table_of_contents: true
---

Here is an up to date list of all the **Mithril Networks**, their configurations and their status:

> Last update: 01/12/2023

| Mithril Network | Cardano Network | Magic Id | Supported | Aggregator Endpoint | Genesis Verification Key | Build From | Note
|------------|------------|:-----------:|:------------:|:-----------------:|:------------------:|:------------:|:------------
| `release-mainnet` | `mainnet` | - | :x: | - | - | - | Not supported yet
| `release-preprod` | `preprod` | `1` | :heavy_check_mark: | [:arrow_upper_right:](https://aggregator.release-preprod.api.mithril.network/aggregator "https://aggregator.release-preprod.api.mithril.network/aggregator") | [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey "https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey") | 🟢<br />**Latest Stable Release** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/releases/latest)  | Stable Release
| `pre-release-preview` | `preview` | `2` | :heavy_check_mark: | [:arrow_upper_right:](https://aggregator.pre-release-preview.api.mithril.network/aggregator "https://aggregator.pre-release-preview.api.mithril.network/aggregator") | [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey "https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey") | 🟠<br />**Latest Stable Pre-Release** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/releases?q=pre-release) | Unstable Pre-Release
| `testing-preview` | `preview` | `2` | :heavy_check_mark: | [:arrow_upper_right:](https://aggregator.testing-preview.api.mithril.network/aggregator "https://aggregator.testing-preview.api.mithril.network/aggregator") | [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey "https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey") | 🔴<br />**Main Branch** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main) | Unstable Testing (devs only)
| `dev-devnet` | `devnet` | `42` | :heavy_check_mark: | [:arrow_upper_right:](http://localhost:8080/aggregator "http://localhost:8080/aggregator") | - | **Any Branch / Release** | Supported on the `devnet` only
| `-` | `testnet` | `1097911063` | :x: | [:arrow_upper_right:](https://aggregator.api.mithril.network/aggregator "https://aggregator.api.mithril.network/aggregator") | [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey "https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey") | - | Decommissioned, not supported anymore

:warning: In this documentation, we use the generic:
* ****YOUR_CARDANO_NETWORK**** identifier, but you need to replace it with the name of the network that runs on your Cardano node (e.g. `preprod`)
* ****YOUR_AGGREGATOR_ENDPOINT**** identifier, but you need to replace it with the endpoint of an aggregator that runs on the Cardano network you target (e.g. `https://aggregator.release-preprod.api.mithril.network/aggregator`)
* ****YOUR_GENESIS_VERIFICATION_KEY**** identifier, but you need to replace it with the genesis verification key url that runs on the Cardano network you target (e.g. `https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey`)