# Protocol Demo / Rust cli

**This is a work in progress** :hammer_and_wrench:

This cli implements a very simple version of the Mithril protocol for demonstration only

---

## Pre-requisites

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).
- Install Build Tools `build-essential` and `m4`. For example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd demo/protocol-demo
```

## Development test and build

```bash
# Test
make test

# Help
make help

# Doc
make doc

# Run with default configuration
make run
```

## Release build and run binary

```bash
# Build
make build

# Help
./mithrildemo --help

# Run
./mithrildemo

# Run with custom configuration
./mithrildemo -k 5 -m 50 --phi-f 0.65 --nparties 5 --nmessages 2
```

## Example output

```
>> Launch Mithril protocol demonstrator with configuration: 
Config {
    m: 100,
    k: 5,
    phi_f: 0.2,
    nparties: 5,
    nmessages: 1,
}

>> Protocol establish phase
Party #0: party created with 826 stakes
Party #1: party created with 741 stakes
Party #2: party created with 144 stakes
Party #3: party created with 734 stakes
Party #4: party created with 41 stakes
Protocol established to StmParameters { m: 100, k: 5, phi_f: 0.2 }

>> Protocol initialize phase:
Verifier: verifier created
Verifier: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Party #0: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Party #1: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Party #2: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Party #3: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Party #4: protocol params updated to StmParameters { m: 100, k: 5, phi_f: 0.2 }
Verifier: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Party #0: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Party #1: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Party #2: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Party #3: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Party #4: protocol keys registration from [(0, 826), (1, 741), (2, 144), (3, 734), (4, 41)]
Artifacts written to artifacts/parties-keys.json

>> Protocol issue certificates phase:
Message #0 to sign: [119, 36, 224, 63, 184, 216, 74, 55, 106, 67, 184, 244, 21, 24, 161, 28]
Party #0: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #0: lottery #6 won
Party #0: lottery #9 won
Party #0: lottery #32 won
Party #0: lottery #43 won
Party #0: lottery #46 won
Party #0: lottery #57 won
Party #0: lottery #89 won
Party #0: lottery #96 won
Party #1: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #1: lottery #6 won
Party #1: lottery #9 won
Party #1: lottery #43 won
Party #1: lottery #46 won
Party #1: lottery #57 won
Party #1: lottery #89 won
Party #1: lottery #96 won
Party #2: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #2: lottery #43 won
Party #2: lottery #46 won
Party #2: lottery #96 won
Party #3: sign message 7724e03fb8d84a376a43b8f41518a11c
Party #3: lottery #6 won
Party #3: lottery #9 won
Party #3: lottery #43 won
Party #3: lottery #46 won
Party #3: lottery #57 won
Party #3: lottery #89 won
Party #3: lottery #96 won
Party #4: sign message 7724e03fb8d84a376a43b8f41518a11c
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
