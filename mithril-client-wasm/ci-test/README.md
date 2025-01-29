# Mithril client wasm: www-test

- A set of tests for the Mithril client WASM library, which is a client for interacting with a Mithril network. The tests cover functionalities provided by the MithrilClient class.

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-client-wasm/
```

- Before running the tests, make sure to install the required dependencies. Use the following command:

```bash
make ci-test-install
```

## Running the tests in the browser

Create an environment file with the following commands and your configuration:

```bash
echo "AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**" > ci-test/.env
echo "GENESIS_VERIFICATION_KEY=**YOUR_GENESIS_VERIFICATION_KEY**" >> ci-test/.env
```

For example with the aggregator on `testing-preview` network:

```bash
echo "AGGREGATOR_ENDPOINT=https://aggregator.testing-preview.api.mithril.network/aggregator" > ci-test/.env
echo "GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey)" >> ci-test/.env
```

Then execute the command to run the tests:

```bash
make ci-test-serve
```

## Test Results Display

The results of each test are displayed in the DOM dynamically. Open [http://localhost:8080](http://localhost:8080) with your browser. (port 8080 is the default port)
