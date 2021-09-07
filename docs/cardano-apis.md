# Cardano APIs

This document lists some useful tools/repositories that could be used by a Mithril node to retrieve information about the chain

* [cardano-node](https://github.com/input-output-hk/cardano-node) is the core component that talks to peers in the Cardano network, retrieves blocks and txs.
  Running a Cardano node is easy enough as there are prebuilt binaries and docker images available, what might be trickier is to configure a node esp. for testing purpose.
  Perhaps [this code](https://github.com/input-output-hk/hydra-poc/blob/master/local-cluster/src/CardanoCluster.hs) could help: It's an implementation in Haskell of a 3-nodes cardano cluster, that should make it "easy" to test the rest of the stack
* [cardano-db-sync](https://github.com/input-output-hk/cardano-db-sync) is a tool that keeps a PostgreSQL database in sync with a cardano network, interacting with a cardano-node to retrieve informations about the current state of the system
  That's probably what one should use to retrieve a UTXO set at some point in time. In particular, there's [this example query](https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/interesting-queries.md#get-historical-utxo-set-for-a-given-timestamp) which seems promising
* [Mantis](https://github.com/input-output-hk/mantis) and [Scorex](https://github.com/hyperledger-labs/Scorex) are tools written in Scala to interact or build a blockchain. Not sure how useful this could be as the Mithril nodes will really just be talking to each others and exchanging messages unrelated to the chain itself.
* [cardano-graphql](https://github.com/input-output-hk/cardano-graphql) is another interesting option: This project provides a [GraphQL]() API on top of a cardano-node which might be simpler to deploy and work with than the db-sync
* [Blockfrost](https://github.com/blockfrost/) provides a REST API on top of Cardano chain, but it's SaaS only. There are however a lot of clients available and this might be a simple option to get started.
