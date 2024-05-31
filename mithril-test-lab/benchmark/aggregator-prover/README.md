# Benchmark aggregator prover route

This tool will run a set of benchmarks (based on [Apache Benchmark](https://httpd.apache.org/docs/2.4/programs/ab.html)) on the aggregator route of an aggregator given a list of transactions. It will produce a CSV file with results of the benchmarks.

First set enviroment variables:
```bash
# Aggregator endpoint
export AGGREGATOR_ENDPOINT=https://aggregator.testing-mainnet.api.mithril.network/aggregator

# Transactions file to prove
export TRANSACTIONS_FILE=transactions-mainnet.txt

# Transactions proved per request range definition
export TRANSACTIONS_PER_REQUEST_MIN=1
export TRANSACTIONS_PER_REQUEST_MAX=3
export TRANSACTIONS_PER_REQUEST_STEP=1

# Apache benchmark total request sent per benchmark
export AB_TOTAL_REQUESTS=1000

# Apache benchmark concurrency level range definition
export AB_CONCURRENCY_MIN=50
export AB_CONCURRENCY_MAX=100
export AB_CONCURRENCY_STEP=50
```

Then, run the benchmarks:
```bash
./benchmark-aggregator-prover.sh
```

Which will output these type of results:
```bash
MITHRIL AGGREGATOR PROVER ROUTE BENCHMARK

>> Aggregator endpoint: https://aggregator.testing-mainnet.api.mithril.network/aggregator
>> Aggregator route: /proof/cardano-transaction
>> Transactions file: transactions-mainnet.txt
>> Transactions available: [100]
>> Transactions per request range: [1 2 3]
>> AB concurrency range: [50 100]
>> AB total requests per run: [1000]
>> AB total runs: 6
>> Output file: benchmark.csv

>> [#1/6] Running stress test with 1000 requests with 1 transactions per request and 50 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (83.23 requests/s)

>> [#2/6] Running stress test with 1000 requests with 1 transactions per request and 100 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (80.68 requests/s)

>> [#3/6] Running stress test with 1000 requests with 2 transactions per request and 50 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (57.50 requests/s)

>> [#4/6] Running stress test with 1000 requests with 2 transactions per request and 100 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (60.61 requests/s)

>> [#5/6] Running stress test with 1000 requests with 3 transactions per request and 50 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (42.86 requests/s)

>> [#6/6] Running stress test with 1000 requests with 3 transactions per request and 100 concurrency
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests
>>>> Success (45.60 requests/s)

>> Benchmark completed:

total_requests,concurrency,transactions/request,requests/s
1000,50,1,83.23
1000,100,1,80.68
1000,50,2,57.50
1000,100,2,60.61
1000,50,3,42.86
1000,100,3,45.60

```