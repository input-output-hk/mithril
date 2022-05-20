# Mithril

<div align="center">
  <a href='https://github.com/input-output-hk/mithril/actions'>
    <img src="https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg" />
  </a>
</div>

Mithril is an research project to provide [Stake-based Threshold Multisignatures](https://iohk.io/en/research/library/papers/mithrilstake-based-threshold-multisignatures/) on top of Cardano network. It's essentially a protocol that allows _stakeholders_ in a Proof-of-Stake blockchain network to individually _sign messages_ and aggregate those signatures in a _certificate_ or _proof_ in such a way that it guarantees stakeholders represent a "random" fraction (the threshold) of total stake. In other words, a malicious participant with less than this fraction won't be able to tamper with the produced certificate.

This repository aims at hosting the various artefacts produced as part of the _prototyping phase_: Documentation, PoCs and prototype code...

* Mithril Aggregator - the server runtime that creates certified blockchain snapshots using Mithril signatures.
* Mithril Signer - the signer runtime that participates to Mithril signatures.
* Mithril Client - a client runtime that checks and restores blockchain certified snapshots.
* Mithril Common - shared library
* Mithril Core - the Mithril cryptographic library

* [Documentation](./docs)
* [Wiki](https://github.com/input-output-hk/mithril/wiki)

## Build

In a shell with a Cargo toolkit in the PATH, just enter

 `make`

 It should build the whole project.

