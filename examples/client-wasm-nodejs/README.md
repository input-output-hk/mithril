# Mithril client wasm library example: Node.js

## Description

This example shows how to implement a Mithril client and use its features in a Node.js environment.

In this example, the client interacts by default with a real aggregator on the network `testing-sanchonet` to:

- Mithril Stake Distribution:
  - list the available Mithril Stake Distributions
  - verify the latest Mithril Stake Distribution
  - verify a certificate chain
  - compute a message
  - verify that the certificate signs the computed message
- Cardano transactions
  - get proof of inclusion for a known set of Cardano transactions
  - verify a certificate chain
  - compute a message for
  - verify that the certificate signs the computed message

## Build and run the example

First you need to compile the Mithril client Wasm library:

```bash
make -C ../../mithril-client-wasm build
```

Then you can run the example:

```bash
npm install
npm run start
```

## Links

- **Developer documentation**: https://mithril.network/doc/manual/develop/nodes/mithril-client-library-wasm
- **NPM**: https://www.npmjs.com/package/@mithril-dev/mithril-client-wasm
