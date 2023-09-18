---
title: Scaling Tribe Workshop Report
authors:
- name: Mithril + Hydra Teams
tags: [news, workshop]
---

From 2023-09-12 to 2023-09-14, most of the _Scaling Tribe_ members, that is the two Hydra and Mithril teams, met for a 3-days long workshop in beautiful French city of Nantes. This page collects various notes, pictures, ideas, thoughts, expectations, ambitions, gathered during the various "sessions" that took place during the workshop.

## A methodological note

Note this workshop was ran following the [Open Space](https://openspaceworld.org/wp2/) technique:
* Bring together a group of committed participants in a space that's both large enough to gather the whole group in _plenary_ sessions and flexible enough to accomodate splitting in sub-groups,
* Give everyone the opportunity to propose "sessions" on topics they want to see addressed by (parts of) the group,
* Collectively build the overall schedule, mixing/splitting sessions as wanted and giving each one a dedicated slot (location + approximate time),
* Let everyone "follow their feet", eg. participate freely in the sessions they are most interested in, without feeling obliged to stay if they don't contribute to or benefit from a session,
* Gather feedback regularly,
* Rinse and repeat every day.

![Scaling Tribe Workshop Attendees](/img/workshop-scaling-tribe.jpeg)

# Sessions Notes & Summary

## Continue "Kupo X Hydra" integration

* We wanted to complete the integration work started [here](https://github.com/CardanoSolutions/kupo/pull/117) in the workshop
* SN started by rebasing the repository which was fairly straight-forward, only
  some conflicts on the `KupoSpec` and how the `endToEnd` spec is set up.
* To allow for some nix-based development, but not requiring a flake.nix/flake.lock on the project, SN is trying to use `use flake "github:input-output-hk/devx#ghc8107-iog"` which worked flawlessly
* The `cabal build` resulted in compiling quite a lot of libraries.. took 2292s.
* While compiling we decided to continue with other sessions and need to continue this work asynchronously.

## Hydra Use cases

## Introduction to Aiken

We were lucky to have _Matthias Benkort_, former member of Hydra team and prolific Cardano open-source contributor, attending our workshop, and we took this opportunity to have an introductory session on [Aiken](https://aiken-lang.org), an alternative Smart Contracts language for Cardano that compiles to Plutus Core which he is [building](https://github.com/aiken-lang/aiken) along with a small team of talented engineers.

TODO: Matthias wanted to share the slides he showed

* During the introduction, SN adds `aiken` to the `hydra` project in anticipation of the next session
* SN realizes that syntax highlighting for emacs would be great.
  - Having the lsp-mode integration in emacs would also be good (second step after syntax).
* SN scratched that itch on the train home: [`aiken-mode`](https://github.com/ch1bo/aiken-mode)

## Hydra Scripts in Aiken

* Being introduced to Aiken is great, but what's better is to experiment it first hand and test how it would feel to program Hydra smart contracts using it.

* As the `hydra-node` just uses the UPLC binary and its hash to construct transactions, we could rely on the same interface, with the difference that the script binary gets produced by `aiken`
* Aiken does produce a socalled blueprint `plutus.json` containing the contract. We staged that file into git and embedded it into a Haskell interface to be able to reference the compiled script as `SerialisedScript` from `hydra-node`.
* Next, we ported one of the simplest contracts we have, namely the [Commit](https://github.com/input-output-hk/hydra/blob/ec6c7a2ab651462228475d0b34264e9a182c22bb/hydra-plutus/src/Hydra/Contract/Commit.hs) validator, [in Aiken](https://github.com/input-output-hk/hydra/blob/4ec572511fc13a526b85efce3aac556ae5bd007c/hydra-plutus/validators/commit.ak)
* After some fiddling with internal representations of data strctures, it came out as a drop-in replacement for the existing script and we were able to run all our tests.
  - Side note: the `plutus.json` embedding was making some problems as `hydra-plutus` was not automatically recompiled. Need to investigate that later.
  - On first try, our test failed with `unListData` and we investigated.
  - Seems like `plutus-tx` is encoding triples as `Constr` data on-chain. So we decode an aiken record as commit datum.
  - Tests were green now right away!
* More importantly, this script gave the Hydra head lifecycle a performance boost (or rather reduced cost) on chain which [materialised](https://github.com/input-output-hk/hydra/pull/1072#issuecomment-1717644108) by doubling the supported number of parties in a Head

## Synergies b/w Hydra & Mithril

* We have similar coding standards, documentation structure. What would be a central place for all important documents?
  * Confluence is private to IOG, mostly relevant for compliance (vendors contract, legal stuff)
* shared folder on Drive?
  * In Hydra, we mostly put documents, organisational stuff, video recordings from Monthly review
  * we do not use confluence anymore, it does not serve a need
* Have a centralised drive and move stuff there that's shared
  * not public, shared across the tribe?
* Media assets
  * logos -> Hydra is trademarked (what does it mean in practice?)
  * people could want to use the logo should they build upon/support a tool
* creating software under https://github.com/input-output-hk organisation or a different one?
  * have a dedicated org? => `cardano-scaling`? => "further opening the repositories"
  * how about sharing website? We use same technology so it should be straightforward, but there's no need and the two products are quite different at the moment. The main interest of a common website is sharing a vision
  * We should be able to share resources for CI (dedicated runners, actions)
* Hydra needs a cardano-node -> but does it need a full node?

How about building a common new tool/product?
* history-less node?
  * generally useful, and paritcularly in the hydra-node case as it does not need past history and needs to start quickly
* have a product/usage centric approach -> focus on users' need
* a light node in the phone?
  * strictly better than lightwallet w/ central operator

![](/img/synergies.jpeg)

## Security vulnerabilities disclosure

* Started generating some `SECURITY.md` using ChatGPT (!). The main problem we have with our current policy revolves around the handling of _silent fixes_ which are expected to be "hidden", with vulnerabilities disclosed very late
* Sharing a conviction about silent fixes:
  * the commonality between all software where there are silent is that _they are complicated too update_ (or upgrade process is significantly painful)
  * consequence: application of security patches will take month, giving ample time to attackers for exploiting vulnerabilities
  * _Therefore_: you need to keep the visibility of security updates very low in order to give time to users
* practically, how do you do that?
  * Example from [ASF](https://www.apache.org/security/committers.html)
    ```
    The project team commits the fix. Do not make any reference that the commit relates to a security vulnerability.

    The project team creates a release that includes the fix.

    After (or at the same time as) the release announcement, the project team announces the vulnerability and the fix.
    ```

    Idea: the fix and release happen at the same time which closes the gap to allow attackers' exploit to happen
  * Note: Reading a CVE does not mean you can exploit the vulnerability right now, it requires some time to build it
  * Example from Hydra: [this
    change](https://github.com/input-output-hk/hydra/commit/2f45529729e28254a62f7a7c8d6649066923ed1f#diff-acbd6a75e65446acdbedda2023a91f15b072d83479648e359121ff802555c09eR325) clearly shows something changed into the validator which gives any attacker a hint on what to do
* Security researchers actually work by scanning commit diffs => any suspicious change will give a lead to an exploit
* It's very easy to verify which script is run by a Hydra head, and at which commit/date it was produced
* Hiding security fixes inside a binary w/o publishing code is worse:
  1. people don't trust arbitrary binaries
  2. it's a huge red flag
* Package managers/OS Distros have a dedicated security stream to handle fixes

* All the 3 vulnerabilities found in Hydra are "counterparty risks"

* What's the impact of a security issue in Hydra: Only the Head, not the whole network
  * not the case for Mithril! => whole network risk because SPOs might not upgrade?
  * There are also "local" risks
  * Using Stake percentage is also a good measure of trust and risk: If you need to update signature scheme (which breaks the most the compatibility)

* Is it related to [Bayesian thinking](https://www.theknowledge.io/bayesian-thinking/)?

* How about adopting standard OSS practice for Hydra? Mithril? What would be the consequences?
  * Standard OSS = vulnerability issue identified -> fix issue through private process -> make public patch and release a new version -> use dedicated security channels for upgrade -> publisH CVE immediately
  * Hydra:
    * worst case consequences on-chain: close the head, restart the head w/ new version
    * better case: upgrade nodes and restart w/o closing the head
    * we need to sign binary releases
    * various dimensions of incompatibilities => qualify the fix, use semantic versioning
    * binary upgrade needs to be _hassle-free_
    * promoting "premature" head closure is good to signal that's something which DApps should always be taking care of
  * promote use of CVSS, could use CVE/version detection in the payment channel use case, with managed heads
* Which communication channels?
  * Check dependabot?
  * Any currently used announcement channels -> Discord

## More decentralised mithril network

* Mithril currently operates with a centralised aggregator that orchestrates the registration and signatures of signers. Why is this a problem?
  * IOG operates it, it's a SPOF and does not scale well => But there are well known ways to make centralised systems scale either through horizontal scaling (sharding keys and signatures handling for example), vertical scaling, replication...
  * It's actually more of a security issues as the aggregator could is a middleman between signers, and while it's "impossible" to produce invalid signatures, it's possible for the aggregator to ignore some signers' registration and lie to signers on who is registered and who is not
  * Having multiple aggregators in the current infrastructure does not make much sense as signers only register with one aggregator. While we could make it possible to support multiple aggregators in the current setting, it does not make much sense and will only complicate the system. we need to move to a decentralised model whereby signers broadcast registrations and signatures, and aggregators can subscribe to those broadcast and offer various services, including the production, maintenance and delivery of snapshot
* The core issue is the sharing of the key registration, which is the most sensitive operation. The upside is that we have 5 days (the duration of an epoch) to have all the SPOs exchange their keys. The downside is that to do it, we need some decentralised exchange medium that every signer or relay can access. Some candidates:
  * A publicly accessible network (eg. Twitter/X?)
  * ipfs?
  * DNS?
  * Cardano network -> see existing design of Mithril-centric mini-protocols from last year
  * dedicated network?
* We need some kind of (BFT) distributed consensus that works over an "open" network and that can converge in 5 days
  * TODO: experiment with decentralised relays over libp2p and some BFT consensus

![](/img/more-decentralised.jpeg)

## Mithril Use cases

We brainstorm and do a quick investigation on possible Mithril use cases:

| Use case                          | Snapshot              | Problem solved                    | Users/Customers |
|-----------------------------------|-----------------------|-----------------------------------|-----------------|
| Bootstrap full node               | Node DB               | Slow sync up w/ chain             | SPOs            |
|                                   |                       |                                   | Hydra/Layer 2   |
| Tx Validation w/o full node       | Protocol Parameters   | UX & DX                           | SDK builders    |
| Rewards calculation w/o full node | Rewards               | Tax Reports                       | Wallets         |
|                                   |                       | Need historical data              |                 |
|                                   |                       | Use DB Sync                       |                 |
| X-Chain bridges                   | UTxO                  | Chain state validation            | Sidechains      |
|                                   |                       |                                   | X-Chain bridges |
|                                   |                       |                                   | Midnight?       |
| Fixed size Blockchain             | Ledger State + blocks | Limitless growth of storage space | ?               |
|                                   |                       | Need to sign same LS              |                 |
| Trustless Software Distribution   | Hash of binaries      | Software Supply chain security    | Builders?       |
|                                   |                       | Which software would be signed?   |                 |

![](/img/mithril-use-cases.jpeg)

## Full SPOs Participation

* How to increase SPOs participation in the Mithril signing process?
* Why do we need that? For security of the protocol
  * Until we reach 80-90% of Cardano stake participation, there is no guarantee that the distributiong of the subset of SPOs participating in Mithril is identical to the mainnet distribution, eg. hypothesis about honest stake distribution at the whole network level do not carry "naturally" for subsets of participants
  * Yet, security in the real world as perceived by users is not black or white, but it's a continuous shade of gray: Some things are perceived as more secure than others, and security needs are also dependent on the _risk_.
* Participation in signing is also tied to _snapshots usage_: If more people want or need to use Mithril, there's a greater incentive for SPOs to participate to provide a safer network and to unlock potential revenue streams from paying users
* Possible actions/ideas to increase participation:
  * Actively "sell" Mithril to SPOs: Engage with large private/public SPOs (IOG, Emurgo, CF, whales) and convince them to run Mithril signer (also a good opportunity to make the experience smooth)
  * Package mithril w/ the cardano-node for automatic deployment => Talk to Operators' Guild?
  * Mithril support in wallets (Lace) will help adoption grow
  * "Slashing"?
  * [Leios](https://github.com/cardano-foundation/CIPs/blob/ouroboros-leios/CIP-0079/leios-design.pdf) will need some Stake-based Threshold multisignature scheme for endorsement certificates, something which can be provided by Mithril

![](/img/more-spos.jpeg)

## Scaling Tribe Handbook

* There appears to be a need for shared documentation at the tribe level, something like a _Scaling Tribe Handbook_
* What are the existing sources of documentation?
  * There's a Confluence scaling page which is used exclusively for _internal stakeholders_
  * Hydra _team_ used to maintain a Confluence space but this has been deprecated
  * There are various documents on Google Drive floating around, for _stakeholders_ and _team_
  * We use Miro intensively for _Team_ work
  * https://mithril.network is the published documentation for Mithril
  * https://hydra.family is the published documentation for Hydra
  * [Hydra wiki](https://github.com/input-output-hk/hydra/wiki) and [Mithril wiki](https://github.com/input-output-hk/mithril/wiki) are used for developer-centric documentation, mostly _Logbooks_
  * Github repositories (notably project boards) are also team and developers centric
* https://documentation.divio.com is a good framework to help categorise documents and structure them.
* Action: Go through existing documents systematically, categorise them according to 2 "dimensions":
  * Target audience: core developers, broader team, contributors, users, operators, builders...
  * documentation category: tutorials, howtos, explanation, reference
  * Ensure each document fits in _one and only one_ category, and otherwise restructure documentation towards that goal

![](/img/tribe-handbook.jpeg)

## Tezos & OCaml presentation

We invited [Xavier van de Woestyne](https://www.linkedin.com/in/xavdw/) as a "guest star" to give a talk and have a chat about his work on [Tezos](https://tezos.com/) in [OCaml](https://ocaml.org/). Tezos is another Proof-of-Stake blockchain with a different consensus called [Tenderbake](https://arxiv.org/abs/2001.11965), an account-based ledger model, and a stack-based smart contracts language called [Michelson](https://tezos.gitlab.io/active/michelson.html).

## Better Mithril Communication w/ Cardano-node

* Mithril currently communicates (or rather extract informations) about the Cardano chain from a running cardano-node using [cardano-cli](https://github.com/input-output-hk/cardano-cli).
* While this works and is reasonably portable, it has some limitations that are not so problematic today but will be in the future, eg. when we want to extract UTxO set for example
* We quickly settled on testing [Pallas](https://github.com/txpipe/pallas/tree/main/pallas-network/src/miniprotocols) as a client

## Spike a "Light node"


What if you started a cardano-node w/ just the last immutable chunk and ledger?:
   * cardano-node crashes when local chain sync protocol asks for a block that we don't have. Tested with kupo connecting to local client socket with some block 19 hours ago...
   * querying a block that's in volatile DB works
   * copying previous chunk which contains the queried block works
   * TODO: make a PR to the node to not crash when you don't find the expected indices? Node does not expect to be queried for slot that does not exist but is supposed to be in the past, so perhaps this does not really make sense?
2. what if you remove the volatile DB?
   * removing completely volatile DB -> nodes starts and recreates volatile folder
   * ledger state is read back correctly then node starts to sync up from latest immutable tip
   * volatile DB is ~50MB so not much saving anyway

Ideas:
* We could write a small program that loads the ledger state (whatever that is) and walks back to the anchored ledger state (eg. LS after applying latest immutable blokc) and serialise and sign that as part of Mithril cert
  * Pb: is that now this LS does not contain any "history" -> needs to be reconstructed when node starts?
* Sign each immutable chunk and download them in demand
  * could use bittorrent -> chunk files _are_ signed anyway
* We might want to sign the block CBOR bytestring, not the chunk files which is a "private" format
  * sign the root of a MT for blocks
  * blocks can be downloaded through the (local or n2n) Chain sync protocol easily and the serialisation format is supposed to be immutable (?)
* Mithril could then serve blocks and ledger state on demand
