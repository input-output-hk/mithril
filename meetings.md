# Meeting Minutes

Meeting minutes are sorted by date, most recent first.

## 2021-10-05 - Mithril Product Q&A

Separate proof production, Mithril signatures of the state from the actual storage and delivery of UTXO
Mithril lightwallet should be able to connect to any (trusted?) node
* need not keep all the certificates -> only need to keep it to some depth

how about verifying proof on a mobile phone?
* This is doable, a cert is about 1-200KB

we need initial trusted point / result of previous certificate

How to boostrap a node with mithril?
* should we be signing past data depending on the stake of the past? But what about keys
* key evolving signature -> key rotation over time => avoid an adversary tracking nodes and resigning past data
* we need to be starting from some definite point in time (Eg. mithril hard-fork?)
* there's some misconceptions floating around that mithril certificate should start at genesis, eg. retrofitting "block headers" in the past
  but it does not seem possible to sign past blocks using past stakes and signatures

## 2021-09-07 - Roman/Dmytrov

Introductions:
* Arnaud is tech architect for Hydra
* Roman worked for IOG for years, reasearch on consensus/ouroboros, work on midnight
* Dmytro involved in the stablecoin project

Current situation:
* Charles asked RO to find a team in Ukraine to implement Mithril node -> IdyllicVision, used to work w/ several types of ledger
* Idyllic vision will implement network/storage, waiting for SoW
* NDA is signed
* AB produced some docs from the paper and discussions

Information needed:
* API to get access to cardano ledger, to get the full chain, interface to cryptographic functions
* Scorex and Mantis frameworks in Scala
* Links to Haskell libraries, entrypoint to read the blockchain?
* Impossible to build the UTXO Set outside of Haskell => need DB Sync to read the UTXO set from PG
* Initially, Mithril node does not need to post TXs to the Cardano node => just read only access so no need to have an API to post txs

## 2021-09-07 - Mithril Protocol Details

### Setup

* group, signature scheme => can be fixed/static for each instance of the protocol
* bulletproof -> requires reference string, something sufficiently random -> hash that

* catalyst => uses hash of header block
* If I select a weird seed string, people would be suspicious -> need to be impredictable

2 different proof systems:
* concatenation
* bulletproof : size much better, 5KB/epoch rather than 200KB, verification non trivial

### Key registration

* overlap with staking, could happen on-chain
  * initially there were thoughts of having mithril signatures be something slot leader does in slot generation

### Operations

* msg = hash of UtxO + hash of stake distribution for next round
* index: k is threshold value, m is maximum index
  * m lotteries, need to "win" k out of m lotteries  to produce a valid multisignature
  * example m = 2500, k = 300
    * every user genertaes a signature
    * does 300 trials, 1 .. 300 -> get `ev` (eligibility evaluation) for each index
=> you broadcast the signature(s) at the index for which ev < threshold
* But we need k signatures to produce a certificate

### Use case

* new client shows up, mithrhil will help them get a new UTxO state
  * Client needs to verify all certificates up to some point
* mithril is not useful for the producers except to agree on the next epoch's distribution

### Details

frequency of signatures: one per epoch, more frequently might be more expensive

how to bootstrap? => does not make sense to retrofit, start from some point in time, some epoch

what's next:
* submission to [PKC](https://pkc.iacr.org/2021/) in 10 days,
* alternative proof system named Halo => might be relevant for mithril
* group setting? signature scheme requires a pairing, proof system requies embedded curve => costly requirements => dense mapping for bulletproof quite unwieldy
* find a smarter mapping

size of 1 signature: 1 group element, but we need the MT to verify the stake distribution => signer needs to provide the path in the MT alongside the signature
* Bulletproof: use poseidon that behaves better with MT, MT are popular with ZK proofs researchers and users
* Could optimise the size of proofs in concatenation scheme

musig is simpler: set of pubkeys is known
* mithril adss the complication of verifying the stake which adds much more information to verify[<0;125;28M[<0;125;28m

## 2021-09-03 - Mithril Product Meeting

Agenda:
* What can we do with Mithril?
* Something has been moved forward with Galois
* Coherent product scope and desired outcome

Discussion revolves mostly about https://input-output.atlassian.net/wiki/spaces/SPS/pages/3057713293/Technical+Architecture, with AB presenting the thing and people reacting/asking questions.
* What about whales running node? => lower threshold
* Rewriting to put it in the node => monetary incentive, better from an engineering and product perspective
  * We don't have the resources right now, esp. as lead architect for node is leaving
  * Be clear about what we expect and define a proper business case
* We don't have a running distribution of the stakes -> issue a snapshot
* Clients (and Mithril signers) wille need the UTxO set
* Can I run this in a browser or a mobile, e.g for a light wallet?
  * What people need is a snapshot at some point in time, not live data
  * A node could provide a subset of UTxOs by signing the hash of those as part of a MerkleTree of known hashes
