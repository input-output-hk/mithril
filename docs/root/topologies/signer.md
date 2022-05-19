 # Signer

 A Signer is a participant to a Mithril Multisignature. As such, a signer assesses that the signed message is the expected genuine content. In this case, the blockchain snapshot is
 
  *  the right one according to the shared snapshot signature
  *  contains the previous snapshot signature

When he has checked these 3 steps, the Signer computes his Mithril vertification key using the shared content (snapshot digest) and sends it to the Aggregator so it will be used in the multisignature process.

 ## Topology

![](images/signer.png)
