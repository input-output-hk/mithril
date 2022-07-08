---
sidebar_position: 4
sidebar_label: Mithril Simulation
---

# Mithril Protocol Simulation

:::info

* Thanks to this demo you will get a better understanding of the **Mithril Protocol**. You will hopefully visualize how the participants interact to create a multi signature and what's the impact of the protocol parameters.

* This simulation is ran by a CLI that you will build and run, and that will ultimately generate **real** Mithril multi signatures!

* For the purpose of reproducibility of the results, the simulation uses a deterministic source of randomness.

* During the simulation some artifacts will be written to an `artifacts` folder, such as `verification keys`, `individual signatures` and `multi signatures`.

:::

## What you'll need

* A Linux (preferred) or a macOS computer.

* A [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.62.0+).

## Download source

Download from Github (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

## Build Mithril Protocol demo binary

Change directory

```bash
cd mithril/demo/protocol-demo
```

Run tests (Optional)

```bash
make test
```

Build executable

```bash
make build
```

## Verify build

Check that the Mithril Client binary is working fine by running its help

```bash
./mithrildemo -h
```

You should see

```
mithrildemo 
Simple demonstration of the Mithril protocol

USAGE:
    mithrildemo [OPTIONS]

OPTIONS:
    -h, --help                     Print help information
    -k, --k <K>                    Quorum parameter [default: 5]
    -m, --m <M>                    Security parameter, upper bound on indices [default: 200]
        --nmessages <NMESSAGES>    Number of messages to sign [default: 1]
        --nparties <NPARTIES>      Number of parties [default: 5]
        --phi-f <PHI_F>            f in phi(w) = 1 - (1 - f)^w, where w is the stake of a
                                   participant [default: 0.2]
```

## Run the simulation

:::tip

A friendly reminder about the protocol parameters:

* `k`: the `Quorum` parameter represents the minimum number of individual signatures (gathered from multiple participants) required to be aggregated in a multi signature.
* `m`: the `Security` parameter represents the total number of `lotteries` in which each participant can participate in order to individually sign the message.
* `phi-f`: the parameter on which depends the probability of a particpant to win a `lottery`. It variates between `0.0` (less chance) and `1.0` (more chance).

:::

:::danger

The `security level` of the protocol is highly dependent on the value of the `protocol parameters`.

Therefore they will be carefully selected by the Mithril cryptographers and researchers in order to guarantee that only legit stakeholders representing a sufficient threshold of the total stakes can combine their individual signatures in a valid multi signature.

:::

### Case 1: Produce a multi signature

Run the simulation wih `5` participants

```bash
./mithrildemo -k 5 -m 50 --phi-f 0.65 --nparties 5
```

The simulation should succeed and produce (or aggregate) a multi signature!

```
>> Launch Mithril protocol demonstrator with configuration: 
Config {
    m: 50,
    k: 5,
    phi_f: 0.65,
    nparties: 5,
    nmessages: 1,
}

>> Protocol establish phase
Party #0: party created with 826 stakes
Party #1: party created with 741 stakes
Party #2: party created with 144 stakes
Party #3: party created with 734 stakes
Party #4: party created with 41 stakes
Protocol established to StmParameters { m: 50, k: 5, phi_f: 0.65 }

>> Protocol initialize phase:
Verifier: verifier created
Verifier: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Party #0: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Party #1: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Party #2: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Party #3: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Party #4: protocol params updated to StmParameters { m: 50, k: 5, phi_f: 0.65 }
Verifier: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #0: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #1: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #2: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #3: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #4: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Artifacts written to artifacts/parties-keys.json

>> Protocol issue certificates phase:
Message #0 to sign: [119, 36, 224, 63, 184, 216, 74, 55, 106, 67, 184, 244, 21, 24, 161, 28]
Party #0: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #0: lottery #2 won
Party #0: lottery #3 won
Party #0: lottery #8 won
Party #0: lottery #13 won
Party #0: lottery #16 won
Party #0: lottery #17 won
Party #0: lottery #19 won
Party #0: lottery #23 won
Party #0: lottery #25 won
Party #0: lottery #28 won
Party #0: lottery #29 won
Party #0: lottery #31 won
Party #0: lottery #42 won
Party #0: lottery #43 won
Party #0: lottery #46 won
Party #1: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #1: lottery #2 won
Party #1: lottery #3 won
Party #1: lottery #8 won
Party #1: lottery #13 won
Party #1: lottery #16 won
Party #1: lottery #17 won
Party #1: lottery #19 won
Party #1: lottery #23 won
Party #1: lottery #25 won
Party #1: lottery #29 won
Party #1: lottery #31 won
Party #1: lottery #42 won
Party #1: lottery #43 won
Party #1: lottery #46 won
Party #2: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #2: lottery #19 won
Party #2: lottery #43 won
Party #2: lottery #46 won
Party #3: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #3: lottery #2 won
Party #3: lottery #3 won
Party #3: lottery #8 won
Party #3: lottery #13 won
Party #3: lottery #16 won
Party #3: lottery #17 won
Party #3: lottery #19 won
Party #3: lottery #23 won
Party #3: lottery #25 won
Party #3: lottery #29 won
Party #3: lottery #31 won
Party #3: lottery #42 won
Party #3: lottery #43 won
Party #3: lottery #46 won
Party #4: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #4: lottery #19 won
Party #0: aggregate signature computed
Party #1: aggregate signature computed
Party #2: aggregate signature computed
Party #3: aggregate signature computed
Party #4: aggregate signature computed
Artifacts written to artifacts/single-signatures.json
Artifacts written to artifacts/multi-signatures.json

>> Protocol verify certificates phase:
Message #0 to verify: 7724e03fb8d84a376a43b8f41518a11c
Party #0: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Verifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Party #1: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Verifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Party #2: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Verifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Party #3: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Verifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Party #4: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!
Verifier: aggregate signature successfully verified for 7724e03fb8d84a376a43b8f41518a11c!

>> Congrats, protocol terminated with success!
```

### Case 2: Does not produce a multi signature

Run the simulation wih `5` participants

```bash
./mithrildemo -k 5 -m 5 --phi-f 0.25 --nparties 5
```

The simulation should fail and not produce (or aggregate) any multi signature!

```
>> Launch Mithril protocol demonstrator with configuration: 
Config {
    m: 5,
    k: 5,
    phi_f: 0.25,
    nparties: 5,
    nmessages: 1,
}

>> Protocol establish phase
Party #0: party created with 826 stakes
Party #1: party created with 741 stakes
Party #2: party created with 144 stakes
Party #3: party created with 734 stakes
Party #4: party created with 41 stakes
Protocol established to StmParameters { m: 5, k: 5, phi_f: 0.25 }

>> Protocol initialize phase:
Verifier: verifier created
Verifier: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Party #0: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Party #1: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Party #2: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Party #3: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Party #4: protocol params updated to StmParameters { m: 5, k: 5, phi_f: 0.25 }
Verifier: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #0: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #1: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #2: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #3: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Party #4: protocol keys registration from [("0", 826), ("1", 741), ("2", 144), ("3", 734), ("4", 41)]
Artifacts written to artifacts/parties-keys.json

>> Protocol issue certificates phase:
Message #0 to sign: [119, 36, 224, 63, 184, 216, 74, 55, 106, 67, 184, 244, 21, 24, 161, 28]
Party #0: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #1: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #2: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #3: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #4: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #0: not enough signatures to compute aggregate
Party #1: not enough signatures to compute aggregate
Party #2: not enough signatures to compute aggregate
Party #3: not enough signatures to compute aggregate
Party #4: not enough signatures to compute aggregate
Artifacts written to artifacts/single-signatures.json
Artifacts written to artifacts/multi-signatures.json

>> Protocol verify certificates phase:
Message #0 to verify: 7724e03fb8d84a376a43b8f41518a11c
Party #0: aggregate signature not found 7724e03fb8d84a376a43b8f41518a11c

>> Certificate verification failed: aggregate signature not found
```

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../mithril/intro.md) section.

:::
