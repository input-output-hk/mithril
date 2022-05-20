 # Signer

 A Signer is a participant to a Mithril Multisignature. As such, a signer assesses that the signed message is the expected genuine content. To do so, the signer computes the digest from the inormation stored on a Cardano node and signs it with his secret key. This signed message with the stake the Signer wants to invest in the signature process is sent to the Aggregator. This way, the Aggegator can assess that the Signer is identified and has the same message. With the Signer Key and the stake information, the Aggregator generates a Verification Key that is used to identify the Signer in the Mithril internal process.

 ## Topology

![](images/signer.png)
