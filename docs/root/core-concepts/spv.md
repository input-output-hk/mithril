# Simple Payment Verification

* SPV has been around in bitcoin since day 1 as a technique to verify inclusion of transactions in blocks without
  verifying the whole block, and it is based on Merkle-tree proofs: Given a block header containing the root hash of the
  block, a proof is a sequence of hashes representing a branch in the Merkle-tree, so verifying the proof amounts to
  verifying that recursively hashing each node in the sequence yields the root hash.
    *
    This [page](https://developer.bitcoin.org/devguide/operating_modes.html?highlight=spv#simplified-payment-verification-spv)
    introduces SPV in contrast with full-node mode
    * From a usage point of view, SPV can be used to answer to the question: "Is transaction with hash `abc` part of
      block `123`?" without having to download the full block. The answer is a proof of inclusion and if positive one
      can then download the full block
* However, to construct the proof one needs to know which transaction it is interested in and the details of the block,
  so this can only be done by _full nodes_, hence SPV clients would need to find another mean to construct proofs which
  is impractical
    * SPV Clients _issuing_ transactions can use SPV to check their inclusion in blocks easily
    * However if one wants to track funds from several wallets (eg. a desktop and a mobile one) then to keep this
      information in sync is difficult
* To solve this problem, [Bloom Filters](https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki) have been
  proposed and implemented early on (the BIP-0037 proposal dates back to 2012) in nodes and in various Lightweight
  clients.
    * Most notably, Android wallets based on [bitcoinj](https://bitcoinj.org/) which are cited in the initial proposal
    * [Electrum](https://electrum.readthedocs.io/en/latest/faq.html#how-does-electrum-work) is another popular wallet
      that uses SPV to verify transactions history. Electrum is [open-source](https://github.com/spesmilo/electrum) and
      coded in Python that also supports Lightning
    * When connecting to a peer node, lightweight clients can create and populate a bloom filter _on the peer side_,
      then the peer checks every block's content against the filter (transaction signer, addresses,...) and send a proof
      of inclusion for transactions that do match.
    * The bloom filter can output _false positives_ so there is some small probability the client will download more
      blocks than needed, eg. retrieve a block that contains a transaction that did match the filter but was
      uninteresting
* It [has been found](https://jonasnick.github.io/blog/2015/02/12/privacy-in-bitcoinj/) quickly those Bloom filters were
  actually problematic:
    * They do not provide privacy as there are easy attacks that can leak the addresses the client is interested in
    * Nodes providing filtering service are susceptible to DoS attacks with specially crafter Bloom filters
* [BIP-0157](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki)
  and [BIP-0158](https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki) have been proposed to mitigate those
  issues. The provide so-called _Block filters_ protocol which works roughly as follow:
    * Nodes can maintain several disting probabilistic compressed filters based on a technique
      called [Golomb-Rice Coding](https://en.wikipedia.org/wiki/Golomb_coding#Rice_coding)
    * Light clients can query _filter headers_ on top of block headers, to know which filters a node maintain and then
      retrieve the filters themselves, checking inclusion of items of interest on the client side
* The [Neutrino](https://github.com/lightninglabs/neutrino) client is the reference implementation, and it
  seems [used on mainnet](https://github.com/lightninglabs/neutrino/issues/225) actively although the README has a
  disclaimer stating the opposite.
* [wasabi](https://docs.wasabiwallet.io/why-wasabi/NetworkLevelPrivacy.html#full-node-by-default-block-filters-over-tor)
  uses block filters over Tor to provide SPV feature while preserving privacy.
  [Bitcoinknots](https://github.com/bitcoinknots/bitcoin) is a fork of bitcoin with a desktop wallet, not sure what the
  differences are
* SPV can also be used for sidechains pegging: https://blockstream.com/sidechains.pdf. The idea is to commit from
  mainchain to sidechain (and back) using SPV proof as part of the transaction so that both chains can verify the
  legitimacy of the coins pegged.

## Generating a proof for some transaction in bitcoind

```
$ bitcoin-cli getbestblockhash
00000000000000000003bdf6945bd7a7bea34585ab227ffc7c67715a363cb0c8
$ bitcoin-cli getblock 00000000000000000003bdf6945bd7a7bea34585ab227ffc7c67715a363cb0c8
....
$ bitcoin-cli getrawtransaction 757faada852390732e7a5594a33c422fe0fe350acd22bc130c6f04834964e1bf true 00000000000000000003bdf6945bd7a7bea34585ab227ffc7c67715a363cb0c8
...
      }
    }
  ],
  "hex": "01000000000102b2d1db9c4e77760c6455b80b2f17dff9a632a6f72f45b369d8fd6a0373371c8d0100000000ffffffff6e441290e472af49e7624602d85cb102ee9e1d1b145dcdf9a95790b38e2d62dd0000000000ffffffff0132ea0a000000000017a914d413cda9ed0f5da0e999faaafb733745b5a34b2f8702483045022100a7ab53c805ec30d3992935ab2377d065a8eddb9ece06a21e0e23546ccf383024022032d464ddba1945e75aa97c1f2e5056e7cc2bead839f89dca737f8f701190d87c0121039034647b3a86556398beef77ce25f6e443e8f91e8bc129f4aa074febaf7c3ddc0247304402202c6f92d6077e4b46388e6ff113b6c08ddfbf205aa35a0b2ca0d79416dc0229a702201a3e2dd5fae3f99eb7a549ae15133c7121ec73814cd13b15abb51ddd6794b9c90121021ed95a235418e076b099f2e6e4c9d5380c5836882efca78b33b7786e4cdd14d500000000",
  "blockhash": "00000000000000000003bdf6945bd7a7bea34585ab227ffc7c67715a363cb0c8",
  "confirmations": 1,
  "time": 1632472393,
  "blocktime": 1632472393
}
$ bitcoin-cli gettxoutproof '["757faada852390732e7a5594a33c422fe0fe350acd22bc130c6f04834964e1bf"]' 00000000000000000003bdf6945bd7a7bea34585ab227ffc7c67715a363cb0c8
04000020d99d3c79b78c541f3a50e1ba50c4b109c4cb8099e3690b0000000000000000009fac806629705a0f86d7d8055b1bd82246feef57d3f53fe09f7374926e94f89e498d4d61ebd00e170d31cfe2e30100000a688e90e8ed22687d743526c05d4dd74a89a2dd25e1b736d2ed323e306a3c35e8d131b41295c4726dcabbe7408ad794aca07692c9e79928061e837009d95bed38cb3070366525cfb69c7c586d778dc169b744c7be38c6cd955cac517fd49eab834226a203d95628b4430caa69d58ef9fced9266d060e95e552bc8ced10b38eed3bfe1644983046f0c13bc22cd0a35fee02f423ca394557a2e73902385daaa7f757c46b30755ceef3948bd3152c2fdef7ff96d8571403dfaccea7f69296b9203974962c02a390fae91fff79fd538f6bd7fcda74f9e95c9fd74785ad11f597558fdc79ba43daf35785eb4376fb54c6f7ea0173b9d671bb64a41b4b2047671f6d1adb8c31b856327a87a20fb72b9a2ff3d6c1e0ccd7a942927bc87362e1a711c21093aef143ad59c7113be896b112074671c1931562e35334ea94b92e5e1325b270503bb3500
```

The later hex-encoded bytestring is the MT proof that the txid is part of the given block.

Then one can verify the proof to retrive the transaction id:

```
$ bitcoin-cli verifytxoutproof $(cat proof)
[
  "757faada852390732e7a5594a33c422fe0fe350acd22bc130c6f04834964e1bf"
]
```

## FAQ

1. Do you see a difference between a SPV node and an SPV client or are they the same thing?

As I understand it, a SPV node is really an SPV-enabled node, eg. a full node that at least can produce SPV proofs on
request, and also can handle client filters whether
through [Bloom filters](https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki)
or [block filters](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki). I am not aware of SPV-only nodes
that would not be clients, eg. leaves in the network, because to produce a proof one has to have the whole block(s) and
not only the headers.

2. What goes in the proof of inclusion or SPV proof?

It's the "branch" of the Merkle Tree that leads to the root which is stored in the block header, ie. a sequence of
transaction hashes (transaction ids) along with their siblings. Given such a branch, one can compute the root hash it
leads to by iteratively computing the hash at each node in the branch, given the previous hashes of the node's children.
If the computed and actual block hashes are the same then the transaction _must_ be in that block.

3. What is the MT protocol?

Are they [Merkle-tree](https://en.wikipedia.org/wiki/Merkle_tree)? I don't know of any _MT protocol_.

4. Is there some more information about the size of an SPV node/ SPV client?

Clients: [This article](https://wiki.bitcoinsv.io/index.php/Simplified_Payment_Verification) and others mention a
current total block headers size of around 50MB. To this cost, one should add the size of the data the client is
interested in, eg. transactions and UTXO set.

5. why is it hard for an SPV client to keep track of numerous wallets?

I haven't read any specific article on this issue but I can imagine that as the number of addresses to track increases,
so does the complexity of checking all of them at every block: Create or retrieve the filters, then retrieve the block(
s) of interest if there's a hit, possibly trying again if it's a false positive...

6. I've read somewhere that not every SPV node keeps a UTXO state, is there more information about how they manage this?

I don't know

7. Do you think this improved Bloom filters is something that we should explorer in the context of Mithril?

Yes. I don't know if it makes sense cryptographically, but if we could in some way derive a certificate for each block
filter, given a global certificate, that would allow lightweight clients and full (or mithril-aware) nodes to interact
through a similar protocol: Get a root certificate, verify it against previously known "Safe" state, then filter the
available UTXO set using deterministic block filters.

8. Why cannot we run plain SPV on Cardano?

Cardano being a Proof-of-Stake chain has different tradeoffs when it comes to validating blocks. In bitcoin, the
Proof-of-Work puzzle is part of the header hence it's easy to limit verification process to headers. In Cardano,
validity of a block depends on current stake distribution and knowing the stake distribution requires going through all
transactions in a block and update the stakes accordingly. Because the stake distribution is large, putting it in block
headers is also impractical.

## Emulating SPV with Mithril

Actually, I think we could not use
the [Merkle-Patricia Tree](https://github.com/input-output-hk/hydra-poc/tree/master/merkle-patricia-tree) we've been
building for Hydra for that purpose:

* Mithril certificate signs the root hash of a MPT built from the UTXO set
    * Note the MPT can be built incrementally at each block/epoch, deleting and adding UTXO
    * Perhaps it would make more sense to store transactions' id in a basic Merkle-Tree
* Mithril nodes maintain block filters
    * We could use Golomb-Rice coded sets as proposed in BIP-0158 to provide compact filters
    * Filters should index all addresses in the UTXO set, at least, to enable light clients to lookup transactions of
      interest only
* Clients can retrieve block filters and certificate, then request specific Transactions/Blocks
    * It's unclear what a light client would need, probably transactions are enough?
* Mithril node sends requested UTXO/Transaction set along with proof it's part of the MPT
    * False positives are simply ignored
    * The proof guarantees the UTXO/Transaction is in the root hence has been signed

I
wrote [some code](https://github.com/input-output-hk/mithril/blob/29b0209ba90d39d02f366ca526485d2474935f5e/test/Mithril/FilterSpec.hs)
experimenting with:

* Storing the UTXO set in a Merkle-Patricia-Tree indexed by reference (transaction id + transaction index)
* Building a Bloom filter for addresses in the UTXO set

Some basic measures I made, given a set of 10000 UTXO:

* Utxo size (serialised) is: 151MB
* Bloom Filter size is: 32KB
* Proof size: 552B
