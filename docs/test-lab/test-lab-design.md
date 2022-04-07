---
sidebar_label: Design
sidebar_position: 2
---

# Mithril Test Lab Design


## Summary 

The overall goal of the Mithril test lab is simple. We wish to develop a framework that
can assess the correctness of Mithril nodes, as well as their resistance to certain
classes of attacks.

The best solution of this problem involves attacking an open question in concurrent systems correctness research: Can we build a unified framework that combines abstract modelling and reasoning with implementation correctness checks or guarantees?

## Goals

The test lab should stand as a method for immediately testing and assessing node implementations. In order, the test-lab should be able to answer the following about a node or collection of nodes:

  - Does it appear to have implemented the necessary functionality?
  - Can it participate correctly in a well-functioning network?
  - Is it resilient against any accidental or random failures?
  - Can it withstand specific attacks agains the protocol or crytography?

The test-lab should be depolyable by anyone who might want to deploy their own node, and programmable by anyone who might want to program a node.
An example of things that a programmer might ask the test lab to do is:

  - Does this network succesfully produce a correct signature in n seconds?
  - If a node lies about their stake, will other nodes still include them in a signature output?
  - If a cryptographic value is replaced, will a valid signature still be produced eventually?
  - What happens if two nodes try to use the same public key?
  - If we adjust the stake held by malicious nodes, at what point are they able to create an invalid signature?

Another use for the test lab is in debugging. In general, concurrent systems are very hard to debug. The test lab should do its best to 
recreate traces of events so that when a failure occurs, a programmer can step through precisely what happened to cause it. If the test-lab
is controllable enough, it might also eventually be able to replay traces in a debug mode, so that the failing implemenation can be observed directly
in the failing condition.

## Architecture 

For the test lab to operate, we need to be able to do the following:

 - Specify a model of the intended behavior of the system
 - (Optional/long term) Check those models and prove correctness
 - Specify attacks or problems that we want to test
 - Present a network layer that can be controlled and observed 
 - Transform the models into observations and actions on the network
 - Orchestrate nodes to run and participate over the network
 - Track the behavior of nodes under test to explain what has gone wrong


```
                   ┌──────────────┐
                   │Model Checkers├───────────────────────┐
                   └──────▲───────┘                       │
                          │                               │
                     Verified by                Can imply safety from
                          │                               │
                    ┌─────┴──────┐                ┌───────▼──────┐
                    │System Model│                │ Attack Model │
                    └─────┬──────┘                └───────┬──────┘
                          │                               │
             Implemented in or layered on     Implemented in or layered on
                          │                               │
┌──────────────────┐  ┌───▼────┐               ┌──────────▼────────┐
│Errors with traces◄──┤Monitors│               │Stream Transformers│
└──────────────────┘  └────────┘               └───────────────────┘
                        Observe                         Control
                          │  ┌──────────────────────┐     │
                          └──►Test Lab Network Layer◄─────┘
                             └─────▲──────────▲─────┘
                                   │          │
                       Drive Behavior    Communicate Over
 ┌────────────────────────────┐    │          │   ┌──────────────────────────────┐
 │Controllable Reference Nodes◄────┘          └───┤Heterogeneous production nodes│
 └────────────────────────────┘                   └──────────────────────────────┘
```

## Model Specification

We have chosen _monitors_ for the underlying representation of the system model. Monitors consume
traces and emit errors if they are discovered. We are not currently certain if this is the best
approach, instead monitors might want to emit streams as well, which would allow for layering of monitors as well as the same underlying representations for attacks/tests and models.

The main benefit of monitors is that they're conceptually straightforward for engineers to understand. A monitor is just a program about a trace. Monitors can be composed in various
ways that should largely be comfortable to programmers. They can be programmed in ways programmers
are familiar with, instead of tying models to e.g. CSP or LTL. This works in our solution because
we will mainly be reasoning over concrete states rather than abstract ones.

While having the full power of Haskell available for monitor creation is a great upside, the downside is that they will not be readily suitable for any form of model checking. State-based representations, for example, provide a more concrete syntax that can be introspected on and reasoned about by model checkers. 

The flexibility of monitors however, could allow us to "split the difference". It should be possible to compile an existing, checkable language such as P, Ivy, or TLA+ into a monitor representation. It would then be possible to model-check the high-level language, but use the monitors for execution.

## Attack Model Specification

We expect to model attacks, or any kind of problem as transformations over the stream travelling over the network. This will capture both random perturbations of data, as well as sophisticated, reactive attacks. Again, it should be possible to layer other languages on top of this abstraction. In general making these languages a domain-specific as possible should also tend to make them more usable.

## Controllable Reference Nodes

A wide range of both correct and incorrect behaviors are required to fully test the implementation of any network protocol. Generating a wide range of correct behaviors can be done by simply creating an impelmentation that we believe to be correct. This implementation can often be implemented in a simpler manner than a production implementation in order to maximise its assurance. The correct implementation can then be used as a reference to check the behaviors and results of other implementations. 

A more complicated problem is how to generate incorrect behaviors. Arbitrary or naive errors are quite easy to generate by transformign or mutating impelementations at the bit level. Such modifications can be effective in finding problems, however they rely on random luck to trigger implementation bugs. Furthermore, reasoning about the wire-representation of messages can be very painful for individuals that are used to thinking about messages as data-structures. In other words, an engineer would rather write "leave field A of Record B out completely" than write "delete bits N..N+L from this message, where N is calcualted from information about the rest of the message". Unfortunately, most correct implementations can make it completely impossible to generate messages that are incorrect, particularly messages that violate data-structure invariants. 

This exposes another requirement of a useful "reference implementation" which is to make it as controllable as possible. That is, it should not only be useable as a correct reference, but it should be able to simulate almost any attacker needed when required, either by generating subtly incorrect messages, or generating correct messages with *meaninguflly* incorrect contents. In the case of Mithril, this could constitute dishonest state representations or cryptograpically difficult values.

An open design question is if this kind of behavior should be built into a controllable reference node or if it can be built as a powerful transformation layer on top of a correct reference node. Keeping them separate results in a more purely correct reference node, which is valuable if it will be used as an implementation reference. Keeping the layers separate *might* result in a less powerful attack capability, or at least higher difficulty in coordinating more sophisticated attacks on the protocol under test.

## Heterogeneous Nodes

A node being tested will need:
 - To be programmed over an abstract network that can be replaced with the test lab network,
 - To have implemented any APIs required by the specification of the protocol it implements,
 - If the protocol specifies any optional API requirements, implementing those will result in better test output

Any node implemented in this way shold be able to be tested by the test lab. There will be an additional per-language requirement for the test-lab to implement bindings to its
network layer in each languages nodes under test will be implemented in. Initially this will be done for Rust and Haskell.

We mention optional APIs because it might be useful for nodes being debugged to present more information about their state. This will allow for more immediate and useful error traces if, for example, a node makes an incorrect decision that would not be immediately observable on the network. In general, such APIs should not require any sensitive information from nodes, in case those APIs are accentially left on in production.

One mode the test lab will be useful in is simply watching a collection of heterogeneous nodes operating and monitoring for any incorrect behavior.
