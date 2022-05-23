# Client

A client is a Snapshot consumer. It is typically a Wallet that wants to synchronize with the Cardano blockchain. When performed from the begining of the chain, it can take several days to compute all the blocks and verify everything is consistent. Snapshots act as certified savepoints, the wallet can assume all the blocks in the snapshots are consistent hence only the newer blocks are to be verified. This dramatically reduces the synchronization times.

## Topology

![](images/client.png)
