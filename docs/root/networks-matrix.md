---
unlisted: true
hide_title: true
hide_table_of_contents: true
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

Here is an updated list of all **Mithril networks**, including their configurations and current statuses:

> Last update: 07/21/2023

<Tabs>
  <TabItem value="mainnet" label="Mainnet" default>

## `release-mainnet`

| Information | -
|------------|------------
| **Mithril network** | `release-mainnet`
| **Cardano network** | `mainnet` 
| **Cardano magic id** |   `-`
| **Supported** | Yes :heavy_check_mark:
| **Status** | Beta 🟢
| **Aggregator endpoint** | `https://aggregator.release-mainnet.api.mithril.network/aggregator` [:arrow_upper_right:](https://aggregator.release-mainnet.api.mithril.network/aggregator)  
| **Genesis verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey)  
| **Era reader adapter type** | `cardano-chain`
| **Era reader address** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/era.addr` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/era.addr)
| **Era reader verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/era.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/era.vkey)
| **Build from** |  **Latest release** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/releases/latest) 

  </TabItem>
  <TabItem value="preprod" label="Preprod">

## `release-preprod`

| Information | -
|------------|------------
| **Mithril network** | `release-preprod` [:mag_right:](https://mithril.network/explorer?aggregator=https%3A%2F%2Faggregator.release-preprod.api.mithril.network%2Faggregator)
| **Cardano network** | `preprod` 
| **Cardano magic Id** |   `1`
| **Supported** | Yes :heavy_check_mark:
| **Status** | Release 🟢
| **Aggregator endpoint** | `https://aggregator.release-preprod.api.mithril.network/aggregator` [:arrow_upper_right:](https://aggregator.release-preprod.api.mithril.network/aggregator)  
| **Genesis verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)  
| **Era reader adapter type** | `cardano-chain`
| **Era reader address** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/era.addr` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/era.addr)
| **Era reader verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/era.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/era.vkey)
| **Build from** |  **Latest release** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/releases/latest) 

  </TabItem>
  <TabItem value="preview" label="Preview">

## `pre-release-preview`

| Information | -
|------------|------------
| **Mithril network** | `pre-release-preview` [:mag_right:](https://mithril.network/explorer?aggregator=https%3A%2F%2Faggregator.pre-release-preview.api.mithril.network%2Faggregator)
| **Cardano network** | `preview` 
| **Cardano magic Id** |   `2`
| **Supported** | Yes :heavy_check_mark:
| **Status** | Pre-release 🟠
| **Aggregator endpoint** | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` [:arrow_upper_right:](https://aggregator.pre-release-preview.api.mithril.network/aggregator)  
| **Genesis verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)  
| **Era reader adapter type** | `cardano-chain`
| **Era reader address** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/era.addr` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/era.addr)
| **Era reader verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/era.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/era.vkey)
| **Build from** |  **Latest pre-release** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/releases?q=pre) 

<br/>

## `testing-preview`
> :warning: For devs only

| Information | -
|------------|------------
| **Mithril network** | `testing-preview` [:mag_right:](https://mithril.network/explorer?aggregator=https%3A%2F%2Faggregator.testing-preview.api.mithril.network%2Faggregator)
| **Cardano network** | `preview` 
| **Cardano magic Id** |   `2`
| **Supported** | Yes :heavy_check_mark:
| **Status** | Unstable 🔴
| **Aggregator endpoint** | `https://aggregator.testing-preview.api.mithril.network/aggregator` [:arrow_upper_right:](https://aggregator.testing-preview.api.mithril.network/aggregator)  
| **Genesis verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey)  
| **Era reader adapter type** | `cardano-chain`
| **Era reader address** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/era.addr` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/era.addr)
| **Era reader verification key** | `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/era.vkey` [:arrow_upper_right:](https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/era.vkey)
| **Build from** |  **Main branch** [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main) 

  </TabItem>
</Tabs>

:::caution
In this documentation, we use the following generic identifiers:

* ****YOUR_CARDANO_NETWORK**** You need to replace this with the name of the network that runs on your Cardano node (eg, `preprod`)
* ****YOUR_AGGREGATOR_ENDPOINT**** You need to replace this with the endpoint of an aggregator that runs on the Cardano network you are targeting (eg, `https://aggregator.release-preprod.api.mithril.network/aggregator`)
* ****YOUR_GENESIS_VERIFICATION_KEY**** You need to replace this with the genesis verification key URL that runs on the Cardano network you are targeting (eg, `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey`)
* ****YOUR_ERA_READER_ADAPTER_TYPE**** You need to replace this with the era reader adapter type used by the Mithril network you are targeting (eg, `cardano-chain`)
* ****YOUR_ERA_READER_ADDRESS**** You need to replace this with the era reader address URL used by the Mithril network you are targeting (eg, `https://raw.githubusercontent.com/input-output-hk/mithril/main/address.addr`)
* ****YOUR_ERA_READER_VERIFICATION_KEY**** You need to replace this with the era reader verification key URL used by the Mithril network you are targeting (eg, `https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_era.vkey`)

:::
