# Mithril Test Lab Design


## Summary 

The overall goal of the Mithril test lab is simple. We wish to develop a framework that
can assess the correctness of Mithril nodes, as well as their resistance to certain
classes of attacks.

The best solution of this problem involves attacking an open question in concurrent systems correctness research: Can we build a unified framework that combines abstract modelling and reasoning with implementation correctness checks or guarantees?

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

We expect to model attacks, or any kind of problem as transformations over the stream travelling over the network. This will capture both random perturbations of data, as well as sophisticated, reactive attacks. 