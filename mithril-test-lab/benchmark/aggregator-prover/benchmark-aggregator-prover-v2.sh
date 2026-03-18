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
    if [[ -n "${1-""}" ]]; then echo "ERROR: ${1}"; echo; fi
    echo "HELP"
    echo "$0"
    echo
    echo "Benchmark a Mithril aggregator prover v2 route"
    echo
    echo "Usage: $0"
    echo
    echo "Available configuration environment variables:"
    echo "- DEBUG: activate debug output"
    echo "- MODE: either 'block' or 'transaction'"
    echo "- AGGREGATOR_ENDPOINT: the aggregator endpoint"
    echo "- TRANSACTIONS_FILE: [mode transaction only] the file containing the transactions hashes"
    echo "- BLOCKS_FILE: [mode block only] the file containing the blocks hashes"
    echo "- ITEMS_PER_REQUEST_MIN: the minimum number of transactions or blocks per request"
    echo "- ITEMS_PER_REQUEST_MAX: the maximum number of transactions or blocks per request"
    echo "- ITEMS_PER_REQUEST_STEP: the step between each number of transactions or blocks per request"
    echo "- AB_TOTAL_REQUESTS: the total number of requests"
    echo "- AB_CONCURRENCY_MIN: the minimum concurrency level"
    echo "- AB_CONCURRENCY_MAX: the maximum concurrency level"
    echo "- AB_CONCURRENCY_STEP: the step between each concurrency level"
    echo
    exit 1;
}

run_stress_test() {
    ITEMS_PER_REQUEST=$1
    ITEMS_PER_REQUEST=$(( ITEMS_PER_REQUEST > 0 ? ITEMS_PER_REQUEST : 1 ))
    AB_TOTAL_REQUESTS=$2
    AB_CONCURRENCY=$3
    AB_CONCURRENCY=$(( AB_CONCURRENCY > 0 ? AB_CONCURRENCY : 0 ))
    AB_TOTAL_REQUESTS=$(( AB_TOTAL_REQUESTS > AB_CONCURRENCY ? AB_TOTAL_REQUESTS : AB_CONCURRENCY ))
    OUT_FILE=$4
    RUN_INDEX=$5
    TOTAL_RUN=$6
    ITEMS_FILE=$7
    TMP_FILE="test.tmp"

    echo ">> [#$RUN_INDEX/$TOTAL_RUN] Running stress test with $AB_TOTAL_REQUESTS requests with $ITEMS_PER_REQUEST ${MODE}s per request and $AB_CONCURRENCY concurrency"
    ITEMS_LIST=$(head -n "$ITEMS_PER_REQUEST" "$ITEMS_FILE" | tr "\n" ",")
    AGGREGATOR_PROVER_URL="${AGGREGATOR_ENDPOINT}${AGGREGATOR_PROVER_ROUTE}?${ROUTE_QUERY_PARAM}=${ITEMS_LIST::-1}"

    if ab -n $AB_TOTAL_REQUESTS -c $AB_CONCURRENCY -s "$AB_TIMEOUT" -H "Accept-Encoding: gzip, deflate, br, zstd" -H "mithril-origin-tag: BENCHMARK" "$AGGREGATOR_PROVER_URL" > "$TMP_FILE" ; then
        REQUESTS_PER_SECOND=$(cat $TMP_FILE | awk '/Requests per second:/ {print $4}')
        if [[ $RUN_INDEX -eq 1 ]] ; then
            echo "total_requests,concurrency,${MODE}s/request,requests/s" >> "$OUT_FILE"
        fi
        echo "$AB_TOTAL_REQUESTS,$AB_CONCURRENCY,$ITEMS_PER_REQUEST,$REQUESTS_PER_SECOND" >> "$OUT_FILE"
        echo ">>>> Success ($REQUESTS_PER_SECOND requests/s)"
    else
        echo ">>>> Failure"
        exit
    fi
    rm -f "$TMP_FILE"
    echo ""
}

# ──────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────
echo ""
echo "MITHRIL AGGREGATOR PROVER ROUTE BENCHMARK"

check_requirements

# Debug mode
if [[ -v DEBUG ]]; then
    set -x
fi

# Check if all required env vars are set
if [[ -z "${MODE}" ]]; then
    display_help "Missing environment variable: MODE, with value either 'block' or 'transaction'"
elif [[ "${MODE}" != "block" ]] && [[ "${MODE}" != "transaction" ]]; then
    display_help "MODE environment variable must be either 'block' or 'transaction'"
fi

if [[ -z "${AGGREGATOR_ENDPOINT}" ]]; then
    display_help "Missing environment variable: AGGREGATOR_ENDPOINT"
fi

if [[ "${MODE}" == "transaction" ]] && [[ -z "${TRANSACTIONS_FILE}" ]]; then
    display_help  "Missing environment variable: TRANSACTIONS_FILE"
fi

if [[ "${MODE}" == "block" ]] && [[ -z "${BLOCKS_FILE}" ]]; then
    display_help  "Missing environment variable: BLOCKS_FILE"
fi

# Set default value for optional variables
ITEMS_PER_REQUEST_MIN="${ITEMS_PER_REQUEST_MIN:-0}"
ITEMS_PER_REQUEST_MAX="${ITEMS_PER_REQUEST_MAX:-100}"
ITEMS_PER_REQUEST_STEP="${ITEMS_PER_REQUEST_STEP:-5}"
AB_TOTAL_REQUESTS="${AB_TOTAL_REQUESTS:-1000}"
AB_CONCURRENCY_MIN="${AB_CONCURRENCY_MIN:-1}"
AB_CONCURRENCY_MAX="${AB_CONCURRENCY_MAX:-100}"
AB_CONCURRENCY_STEP="${AB_CONCURRENCY_STEP:-10}"
AB_TIMEOUT="${AB_TIMEOUT:-180}"

if [[ -z "${OUT_FILE}" ]]; then
    OUT_FILE="${MODE}-benchmark.csv"
    rm -f "$OUT_FILE"
fi
mkdir -p "$(dirname "$OUT_FILE")"

# Set mode dependent variables
if [[ "${MODE}" == "transaction" ]]; then
    ITEMS_FILE=$TRANSACTIONS_FILE
    ITEMS=$(tr "\n" " " < "$TRANSACTIONS_FILE")
    AGGREGATOR_PROVER_ROUTE="/proof/v2/cardano-transaction"
    ROUTE_QUERY_PARAM="transaction_hashes"
elif [[ "${MODE}" == "block" ]]; then
    ITEMS_FILE=$BLOCKS_FILE
    ITEMS=$(tr "\n" " " < "$BLOCKS_FILE")
    AGGREGATOR_PROVER_ROUTE="/proof/v2/cardano-block"
    ROUTE_QUERY_PARAM="block_hashes"
fi

# Run aggregator benchmark over a range of item and concurrency levels
ITEMS_AVAILABLE=$(echo "$ITEMS" | wc -w | xargs)
ITEMS_PER_REQUEST_MAX=$(( ITEMS_AVAILABLE < ITEMS_PER_REQUEST_MAX ? ITEMS_AVAILABLE : ITEMS_PER_REQUEST_MAX ))
ITEMS_PER_REQUEST_RANGE=$(seq -s ' ' "$ITEMS_PER_REQUEST_MIN" "$ITEMS_PER_REQUEST_STEP" $ITEMS_PER_REQUEST_MAX)
AB_CONCURRENCY_RANGE=$(seq -s ' ' "$AB_CONCURRENCY_MIN" "$AB_CONCURRENCY_STEP" "$AB_CONCURRENCY_MAX")
ITEMS_PER_REQUEST_RANGE_LENGTH=$(wc -w <<< "$ITEMS_PER_REQUEST_RANGE")
AB_CONCURRENCY_RANGE_LENGTH=$(wc -w <<< "$AB_CONCURRENCY_RANGE")
TOTAL_RUN=$(( ITEMS_PER_REQUEST_RANGE_LENGTH * AB_CONCURRENCY_RANGE_LENGTH ))

echo
echo ">> Mode: $MODE"
echo ">> Aggregator endpoint: $AGGREGATOR_ENDPOINT"
echo ">> Aggregator route: $AGGREGATOR_PROVER_ROUTE"
echo ">> ${MODE^}s file: $ITEMS_FILE"
echo ">> ${MODE^}s available: [$ITEMS_AVAILABLE]"
echo ">> ${MODE^}s per request range: [$ITEMS_PER_REQUEST_RANGE]"
echo ">> AB concurrency range: [$AB_CONCURRENCY_RANGE]"
echo ">> AB total requests per run: [$AB_TOTAL_REQUESTS]"
echo ">> AB total runs: $TOTAL_RUN"
echo ">> Output file: $OUT_FILE"
echo

RUN_INDEX=1
for ITEMS_PER_REQUEST in $ITEMS_PER_REQUEST_RANGE; do
    for AB_CONCURRENCY in $AB_CONCURRENCY_RANGE; do
        run_stress_test "$ITEMS_PER_REQUEST" "$AB_TOTAL_REQUESTS" "$AB_CONCURRENCY" "$OUT_FILE" $RUN_INDEX "$TOTAL_RUN" "$ITEMS_FILE"
        RUN_INDEX=$(( RUN_INDEX + 1))
    done
done

echo ">> Benchmark completed:"
echo ""
cat "$OUT_FILE"
