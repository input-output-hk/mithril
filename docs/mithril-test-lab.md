# Mithril Test Lab

The following picture describes in high-level box-and-wire diagram how a Mithril Test Lab would instrument Mithril nodes to run complex adversarial tests.

![](./images/mithril-test-lab.jpg)

## Why?

Mithril is both:
1. A somewhat complex protocol to produce Stake based Threshold Multi-signatures. It depends on relatively sophisticated cryptographic algorithms, and mathematical proofs defining security properties and requirements,
2. A set of nodes which run the protocol over a dedicated p2p network, producing signatures, certificates and delivering relevant data to clients.

As the goal of building Mithril network is to provide trusted data to clients without requiring them to run the full chain themselves, it is necessary to ensure the basic properties of the "abstract" protocol are indeed provided by the "concrete" implementation. We plan to reach this goal by developing a _Model-based testing framework_ for Mithril that should be able to check implementation of actual nodes against expected behaviour, under various scenarios and load from "happy path" to "adversarial conditions".

The _Mithril Test Lab_ should be as agnostic as possible about the actual implementation of the nodes.

## What?

The Mithril Test Lab is expected to provide the tooling to build, run, and check the result of those tests. It should be able to:

- Deploy and run a set of Mithril (and Cardano) nodes from some binary packages,
- Control the network through which Mithril nodes are interacting in order to observe the flow of messages related to the Mithril protocol, and intercept them in order to inject delays, reorderings, faults and arbtitrary messages,
- Drive the system from the outside, injecting transactions into the instrumented Cardano network and behaving as clients, interacting with Mithril nodes through their exposed public API,
- Impersonate one or more "normal" Mithril nodes and run_Adversarial_ nodes, trying to "game" the system under various conditions of stakes and degrees of control.

## References

* [Jepsen](https://jepsen.io/) is an implementation of such a MBT framework for verifying _linearizability_ of consensus protocols
* [quickcheck-state-machine](https://github.com/stevana/quickcheck-state-machine)
* [QuickStrom](https://quickstrom.io)
* [Plutus testing framework](https://plutus.readthedocs.io/en/latest/)
