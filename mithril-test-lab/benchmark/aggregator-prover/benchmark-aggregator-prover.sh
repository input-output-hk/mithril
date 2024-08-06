#!/bin/bash

set -e

check_requirements() {
    which ab >/dev/null ||
        error "It seems 'ab' is not installed or not in the path.";
        
}

error() {
    echo 
    echo "ERROR: $1";
    exit 1;
}

display_help() {
    echo
    if [ -n "${1-""}" ]; then echo "ERROR: ${1}"; echo; fi
    echo "HELP"
    echo "$0"
    echo
    echo "Benchmark a Mithril aggregator prover route"
    echo
    echo "Usage: $0"
    echo
    echo "Available configuration environment variables:"
    echo "- DEBUG: activate debug output"
    echo "- AGGREGATOR_ENDPOINT: the aggregator endpoint"
    echo "- TRANSACTIONS_FILE: the file containing the transactions hashes"
    echo "- TRANSACTIONS_PER_REQUEST_MIN: the minimum number of transactions per request"
    echo "- TRANSACTIONS_PER_REQUEST_MAX: the maximum number of transactions per request"
    echo "- TRANSACTIONS_PER_REQUEST_STEP: the step between each number of transactions per request"
    echo "- AB_TOTAL_REQUESTS: the total number of requests"
    echo "- AB_CONCURRENCY_MIN: the minimum concurrency level"
    echo "- AB_CONCURRENCY_MAX: the maximum concurrency level"
    echo "- AB_CONCURRENCY_STEP: the step between each concurrency level"
    echo
    exit 1;
}

echo ""
echo "MITHRIL AGGREGATOR PROVER ROUTE BENCHMARK"

check_requirements

# Debug mode
if [ -v DEBUG ]; then
    set -x
fi

AGGREGATOR_PROVER_ROUTE="/proof/cardano-transaction"

# Check if all env vars are set 
if [ -z "${AGGREGATOR_ENDPOINT}" ]; then
    display_help "Missing environment variable: AGGREGATOR_ENDPOINT"
fi

if [ -z "${TRANSACTIONS_FILE}" ]; then
    display_help  "Missing environment variable: TRANSACTIONS_FILE"
fi

if [ -z "${TRANSACTIONS_PER_REQUEST_MIN}" ]; then
    TRANSACTIONS_PER_REQUEST_MIN=0
fi

if [ -z "${TRANSACTIONS_PER_REQUEST_MAX}" ]; then
    TRANSACTIONS_PER_REQUEST_MAX=100
fi
TRANSACTIONS=$(tr "\n" " " < "$TRANSACTIONS_FILE")
TRANSACTIONS_AVAILABLE=$(echo "$TRANSACTIONS" | wc -w | xargs)
TRANSACTIONS_PER_REQUEST_MAX=$(( TRANSACTIONS_AVAILABLE < TRANSACTIONS_PER_REQUEST_MAX ? TRANSACTIONS_AVAILABLE : TRANSACTIONS_PER_REQUEST_MAX ))

if [ -z "${TRANSACTIONS_PER_REQUEST_STEP}" ]; then
    TRANSACTIONS_PER_REQUEST_STEP=5
fi

if [ -z "${AB_TOTAL_REQUESTS}" ]; then
    AB_TOTAL_REQUESTS=1000
fi

if [ -z "${AB_CONCURRENCY_MIN}" ]; then
    AB_CONCURRENCY_MIN=0
fi

if [ -z "${AB_CONCURRENCY_MAX}" ]; then
    AB_CONCURRENCY_MAX=100
fi

if [ -z "${AB_CONCURRENCY_STEP}" ]; then
    AB_CONCURRENCY_STEP=10
fi

if [ -z "${AB_TIMEOUT}" ]; then
    AB_TIMEOUT=180
fi

if [ -z "${OUT_FILE}" ]; then
    OUT_FILE="benchmark.csv"
    rm -f $OUT_FILE
fi
mkdir -p "$(dirname "$OUT_FILE")"

# Run stress test
RUN_STRESS_TEST() {
    AGGREGATOR_ENDPOINT=$1
    TRANSACTIONS_PER_REQUEST=$2
    TRANSACTIONS_PER_REQUEST=$(( TRANSACTIONS_PER_REQUEST > 0 ? TRANSACTIONS_PER_REQUEST : 1 ))
    AB_TOTAL_REQUESTS=$3
    AB_CONCURRENCY=$4
    AB_CONCURRENCY=$(( AB_CONCURRENCY > 0 ? AB_CONCURRENCY : 0 ))
    AB_TOTAL_REQUESTS=$(( AB_TOTAL_REQUESTS > AB_CONCURRENCY ? AB_TOTAL_REQUESTS : AB_CONCURRENCY ))
    OUT_FILE=$5
    INDEX_RUN=$6
    TOTAL_RUN=$7
    TRANSACTIONS_FILE=$8
    TMP_FILE="test.tmp"
    echo ">> [#$INDEX_RUN/$TOTAL_RUN] Running stress test with $AB_TOTAL_REQUESTS requests with $TRANSACTIONS_PER_REQUEST transactions per request and $AB_CONCURRENCY concurrency"
    TRANSACTIONS_LIST=$(head -n $TRANSACTIONS_PER_REQUEST "$TRANSACTIONS_FILE" | tr "\n" ",")
    AGGREGATOR_PROVER_URL="${AGGREGATOR_ENDPOINT}${AGGREGATOR_PROVER_ROUTE}?transaction_hashes=${TRANSACTIONS_LIST::-1}"
    if ab -n $AB_TOTAL_REQUESTS -c $AB_CONCURRENCY -s "$AB_TIMEOUT" "$AGGREGATOR_PROVER_URL" > $TMP_FILE ; then
        REQUESTS_PER_SECOND=$(cat $TMP_FILE | awk '/Requests per second:/ {print $4}')
        if [[ $INDEX_RUN -eq 1 ]] ; then
            echo "total_requests,concurrency,transactions/request,requests/s" >> "$OUT_FILE"
        fi
        echo "$AB_TOTAL_REQUESTS,$AB_CONCURRENCY,$TRANSACTIONS_PER_REQUEST,$REQUESTS_PER_SECOND" >> "$OUT_FILE"
        echo ">>>> Success ($REQUESTS_PER_SECOND requests/s)"
    else
        echo ">>>> Failure"
        exit
    fi
    rm -f $TMP_FILE
    echo ""
}

# Run aggregator benchmark over a range of transactions and concurrency levels
TRANSACTIONS_PER_REQUEST_RANGE=$(seq -s ' ' "$TRANSACTIONS_PER_REQUEST_MIN" "$TRANSACTIONS_PER_REQUEST_STEP" $TRANSACTIONS_PER_REQUEST_MAX)
AB_CONCURRENCY_RANGE=$(seq -s ' ' "$AB_CONCURRENCY_MIN" "$AB_CONCURRENCY_STEP" "$AB_CONCURRENCY_MAX")
TRANSACTIONS_PER_REQUEST_RANGE_LENGTH=$(( $(echo "$TRANSACTIONS_PER_REQUEST_RANGE" | grep -o " " | wc -l) + 1 ))
AB_CONCURRENCY_RANGE_LENGTH=$(( $(echo "$AB_CONCURRENCY_RANGE" | grep -o " " | wc -l) + 1 ))
TOTAL_RUN=$(( TRANSACTIONS_PER_REQUEST_RANGE_LENGTH * AB_CONCURRENCY_RANGE_LENGTH ))
echo
echo ">> Aggregator endpoint: $AGGREGATOR_ENDPOINT"
echo ">> Aggregator route: $AGGREGATOR_PROVER_ROUTE"
echo ">> Transactions file: $TRANSACTIONS_FILE"
echo ">> Transactions available: [$TRANSACTIONS_AVAILABLE]"
echo ">> Transactions per request range: [$TRANSACTIONS_PER_REQUEST_RANGE]"
echo ">> AB concurrency range: [$AB_CONCURRENCY_RANGE]"
echo ">> AB total requests per run: [$AB_TOTAL_REQUESTS]"
echo ">> AB total runs: $TOTAL_RUN"
echo ">> Output file: $OUT_FILE"
echo

INDEX_RUN=1
for TRANSACTIONS_PER_REQUEST in $TRANSACTIONS_PER_REQUEST_RANGE; do 
    for AB_CONCURRENCY in $AB_CONCURRENCY_RANGE; do
        RUN_STRESS_TEST "$AGGREGATOR_ENDPOINT" "$TRANSACTIONS_PER_REQUEST" "$AB_TOTAL_REQUESTS" "$AB_CONCURRENCY" "$OUT_FILE" $INDEX_RUN "$TOTAL_RUN" "$TRANSACTIONS_FILE"
        INDEX_RUN=$(( INDEX_RUN + 1))
    done 
done 

echo ">> Benchmark completed:"
echo ""
cat "$OUT_FILE"


