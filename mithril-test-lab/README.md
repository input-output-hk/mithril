# Mithril Test Lab

Haskell components and tests for the Mithril system.

# Installing

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
