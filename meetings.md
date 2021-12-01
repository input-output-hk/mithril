# Meeting Minutes

Meeting minutes are sorted by date, most recent first.

## 2021-12-01 - Weekly

> Hello,
> Here comes agenda for today meeting:
> 1. Mithril node status discussion:
>     - Private network setup (WIP).
>     - Resolve comments on github PR's (WIP).
>     - Docker composer for Mithril node
>     - Switch to GraphQL
>     - Working on the Mithril client
>     - Start performance tests
>     - Implement github autotests
> 2. Additional part-time engineer for the project to help Sasha with the project backlog.
> 3. Misc

PR's comments:
- `ulonglong` issue: Seems like it's a Linux specific problem? Replacing with `ulong` does not work on Mac OS
  => file an issue
- DevOps engineer looking at Docker -> perhaps Monday?
- launching a testnet -> https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet
- launching network + docker-compose => next week
- Help on spinning up a devnet network => write a script to do that and maybe inject transactions

TODOs:
- (AB): Look at integer sizes discrepancies: https://github.com/input-output-hk/mithril/issues/64
- (AB, optional): Provide script/code to spin up easily a devnet/testnet: https://github.com/input-output-hk/mithril/issues/65
- (IV): End-to-end scenario with mithril client validating certificate(s)
- (IV): dockerizing node

## 2021-11-17 - Demo

- 3 nodes for the demo, connect to each other directly using [libp2p](https://libp2p.io/) for connecting the nodes
- DB stores the certificates -> can retrieve certificates from API with block number + MT root for UTXO + multisig
- multisign = base64 of binary code from library, how about what happens in client?
  - provide a better representation for aggregate?
  - mithril has multisig + STMs -> the one that rely on the MT, contains the paths of the MT
  - we want to define the serialisation format but not structure it when we send it
- not clear how the library works?
  - define criteria defining when the test should pass
  - explanation: signers will play a lottery, parameter defining how many signatures are required => there might be some messages where the parties don't win enough lottery
    - more parties with all parameters being equal => more succesful signatures
    - inigo working on benchmarks varying the parameters
- next steps:
  - fix bugs -> file issues
  - what kind of benchmarks we need -> brainstorm
  - finish the API
  - node live check
  - aggregate process
  - write documentation
  - build instructions + CI
  - end-to-end test

- actions:
  - (IV) file bugs
  - (IOG) Fix bugs
  - (IV) provide a PR with go code and build/run instructions
  - (IV) internal brainstorm on benchmarks
  - (both) reconvene on Mon/Tue to plan next sprint


## 2021-11-16 - Mithril Dev Q&A

- deployment strategy? CI mandatory
  example testnet: https://github.com/input-output-hk/cardano-node/tree/master/scripts/byron-to-alonzo
  even better packaged: https://github.com/woofpool/cardano-private-testnet-setup
  => why I said we need to interact more...!
- security? =>
- sizing? => amount of Utxo
- persisting keys? => Mithril keys for multisig
  keys can be serialised and exported but cannot be passed
  initialiser can be serialised with the initial keys
  provide an API to be able to update the keys? -> better to update the stake
- process: load the initialiser if it exists, else generate new one
- leave stake cahnge aside for the moment
- getting errors from "not enough indices"
  phi_f = 1 => all indices are valid
  not enough indices is fine, we can tweak phi_f to reduce it
- don't need to keep the individual signatures of each node around -> aggregated by the aggregator
- how does the mithril client verifies a certificate?
  Clerk initialises all parameters
  verifier of aggregate verifies signatures wrt to Clerk
- hardcoded elements?
  leadership? -> height of nodes? => we don't need a leader to start the signing the process
  the entity generating the certificate is not trusted => use
- implementing MT => 1M Utxo = 1GB
- now understanding the library and can start asking questions!

## 2021-11-10 - Parallel Epoch Validation

Parallel Epoch Validation: something Mithril does well without any engineering

* we are verifying message according to some state => verifying several messages against several states
* each message includes the next state
  state is very small : H(MT) + some parameters that don't change often
* things are hard if we have a large state -> implies we need to attach large (n,n+1) state in each verification
* parallelism should be in the node -> state used for verification is very small
* useful for catching up with a chain of certificates -> we can download a whole bunch of certificates w/ proofs and verify all of them in parallel
* we need to measure concretely!!

* do we need the crypto library for verification?
  => probably yes
* we could skip some part of the logic -> use wasm to ship it to light client
* separate library in 2 parts, signer + verifier and only ship the verifier for light clients

## 2021-10-27 - Discussion w/ Cardano node

2 problems:
- end-users have a hard time folllowing the chain
- enterprise users need to resync db-sync every so often which requires hours/days
  - additional ops requirements
  - majority of people running db-sync don't care about history of chain -> make a transaction, get some level of certainty it got "validated"
  - "eventual history" is good enough
  - low-hanging fruit, 1st increment?
- diff. between mithril node/client
  - mithril takes whole Utxo set -> do not solve hardware solutions for "light" clients?
- Q. how long is too long?
  - days... => might be enough to sync up faster with db-sync
  - start w/ current Utxo snapshot then continue from some point (checkpointing)
  - bootstrap the node itself is hard
  - < 6 hours
- real questoin: how fast can I get node and db-sync up and running to be able to make requests on them?
- given an up-to-date node -> have a db-sync w/o history but w/ Utxo set
  - don't need to replay the full history to populate db-sync
  - requires change to db schema -> it does not store Utxo, only a way to replay them (history)
  - requires analysis about queries
- what people want is _mithril_ !
- db-sync is just slow right now, not doing any validation, only populating data
- could use mithril to get some state
- majority of clients use combination of node + db-sync + graphql api
- 2 modes of mithril:
  - get me a validated point -> I want to spend some point
  - get me a validated point + sync the chain in the background (takes hours to download history, particluar choice of DB, lookups instead of writes...)
- mithril in and of itself is not very useful in the enterprise setting -> need more work to do to use the blessed Utxo state
- one use-case: Exchange that use WBE could benefit directly from this -> put the Utxo
- requirements on trust?
  - provide "blessed" checkpoints to exchanges as long as it's quick? -> don't know
- why is it too long?
  - every offline day -> 50M loss for an exchange!
  - every upgrade -> resyncing the whole state
  - more a DevOps best practice problem -> we could provide solutions/advices on how to operate stuff
- let's the PoC/demonstrator live its life until December
  - continue work on the Product side -> have high-level requirements/architecture by EOY and then do proper product design
- why l2 solution wont' work
  - checkpoint approach is cool, great feature to add
  - difficulty : _what_ do you sign? Need a ledger state that's useful -> hash(MT(Utxo))
  - every node has to maintain exact same representation -> not an impl detail anymore, part of the on-chain format -> standardize the format of the state
  - specification ---> implementation dependency
  - have to be part of the ledger's rules, enforced
  - is it so much work to do it on L1?

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
