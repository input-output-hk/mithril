# Mithril client wasm: www-test

* A set of tests for the Mithril client WASM library, which is a client for interacting with a Mithril network. The tests cover functionalities provided by the MithrilClient class.

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-client-wasm/
```

* Before running the tests, make sure to install the required dependencies. Use the following command:

```bash
make www-test-install
```

## Running the tests in the browser

```bash
make www-test-serve
```

## Test Results Display

The results of each test are displayed in the DOM dynamically. Open [http://localhost:8080](http://localhost:8080) with your browser. (port 8080 is the default port)