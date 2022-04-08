# Mithril Test Lab

Haskell components and tests for the Mithril system.

# Installing

## Using Nix

We provide a `shell.nix` to set up a development environment. So a simple call to nix-shell should put everything in place for building, testing, and general development.
Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```nix
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Also, some of us use `direnv` and `nix-direnv` to automatically import & cache the nix-shell environment into our favorite shell or editor.
Within the nix-shell, `cabal build` and `cabal test` should work as expected.

You can also use `nix-build` to build the project and all executables. You will find them in `result/bin/` after the build.

## Using Cabal

1. Install basic Haskell development environment, for example using ghcup. Hydra requires GHC 8.10.7 and a recent cabal (> 3.0).
1. Install various system dependencies On Debian-like:

    ```
    sudo apt install -y  build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
    sudo apt install -y  libz-dev liblzma-dev libzmq3-dev pkg-config libtool
    ```

    Do not confuse `lzma` with `liblzma-dev`, those are 2 existing package!
1. Install forked libsodium

    ```
    git clone https://github.com/input-output-hk/libsodium
    cd libsodium/
    git checkout 66f017f16633f2060db25e17c170c2afa0f2a8a1
    ./autogen.sh
    ./configure
    make && sudo make install
    ```
1. Build and test everything:

    ```
    cabal build all && cabal test all
    ```

# Usage

## Cardano devnet

The `mithril-end-to-end` package provides a `start-devnet` executable that can be used to, well, start a _devnet_. This is a set of 3 interconnected cardano nodes running in BFT (_Byzantine Fault Tolerant_) mode (no pools).

To run,

```
$ cabal run start-devnet
{"contents":[1,{"contents":"RawCommand \"cardano-node\" [\"run\",\"--config\",\"configuration.json\",\"--topology\",\"topology.json\",\"--database-path\",\"db\",\"--socket-path\",\"node.socket\",\"--port\",\"6336\",\"--byron-signing-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1/delegate-keys.000.key\",\"--byron-delegation-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1/delegation-cert.000.json\",\"--shelley-operational-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1/opcert1.cert\",\"--shelley-kes-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1/delegate1.kes.skey\",\"--shelley-vrf-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1/delegate1.vrf.skey\"]","tag":"MsgNodeCmdSpec"}],"tag":"MsgFromNode"}
{"contents":{"stateDirectory":"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-1","nodeId":1,"ports":{"peers":[30488,43178],"ours":6336},"systemStart":"2022-04-08T14:23:32.848282592Z"},"tag":"MsgNodeStarting"}
{"contents":[2,{"contents":"RawCommand \"cardano-node\" [\"run\",\"--config\",\"configuration.json\",\"--topology\",\"topology.json\",\"--database-path\",\"db\",\"--socket-path\",\"node.socket\",\"--port\",\"30488\",\"--byron-signing-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2/delegate-keys.001.key\",\"--byron-delegation-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2/delegation-cert.001.json\",\"--shelley-operational-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2/opcert2.cert\",\"--shelley-kes-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2/delegate2.kes.skey\",\"--shelley-vrf-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2/delegate2.vrf.skey\"]","tag":"MsgNodeCmdSpec"}],"tag":"MsgFromNode"}
{"contents":{"stateDirectory":"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-2","nodeId":2,"ports":{"peers":[6336,43178],"ours":30488},"systemStart":"2022-04-08T14:23:32.848282592Z"},"tag":"MsgNodeStarting"}
{"contents":[3,{"contents":"RawCommand \"cardano-node\" [\"run\",\"--config\",\"configuration.json\",\"--topology\",\"topology.json\",\"--database-path\",\"db\",\"--socket-path\",\"node.socket\",\"--port\",\"43178\",\"--byron-signing-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3/delegate-keys.002.key\",\"--byron-delegation-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3/delegation-cert.002.json\",\"--shelley-operational-certificate\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3/opcert3.cert\",\"--shelley-kes-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3/delegate3.kes.skey\",\"--shelley-vrf-key\",\"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3/delegate3.vrf.skey\"]","tag":"MsgNodeCmdSpec"}],"tag":"MsgFromNode"}
{"contents":{"stateDirectory":"/home/curry/mithril/mithril-test-lab/mithril-end-to-end/devnet/node-3","nodeId":3,"ports":{"peers":[6336,30488],"ours":43178},"systemStart":"2022-04-08T14:23:32.848282592Z"},"tag":"MsgNodeStarting"}
Press enter to quit
```

The nodes' configurations, socket, and db files will live in the `devnet` directory under subdirectories `node-1`, `node-2`, and `node-3`.
