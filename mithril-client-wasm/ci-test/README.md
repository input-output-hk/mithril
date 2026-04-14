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

Specify blocks and/or transactions hashes to certify using comma separated values (examples with `testing-preview` hash) :

```bash
echo "TRANSACTIONS_HASHES_TO_CERTIFY=70606a33c7a140e8fa91095ffa79b8a5c2e0514dbc06a5cc0e5aaffee92665c4,ca177d5f6bc63ff72bb346594be0a8b5fccd2d7ca5066e9b083a449bb6dcec1a,74a0639c898b61cddb52795bca931ff54bad7e5ddb2eb82067bb7a97087bc053" >> ci-test/.env
echo "BLOCKS_HASHES_TO_CERTIFY=e722eec9d464c576274f803cd38569fdba34f22bd404a5a35bb705d8b6f4b44b,c33a8b9a9a261bb6b11df976c404bfca94622c0326c5fbdc90a7af06074115f3,5118997b31cc5fe2ad2f15277f6707f08bbbab54d7786cf0642657a14a838720" >> ci-test/.env
```

Then execute the command to run the tests:

```bash
make ci-test-serve
```

## Test Results Display

The results of each test are displayed in the DOM dynamically. Open [http://localhost:8080](http://localhost:8080) with your browser. (port 8080 is the default port)
