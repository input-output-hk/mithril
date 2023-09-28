#!/bin/bash

set -e

# Debug mode
if [ -v DEBUG ]; then
    set -x
fi

# Check if all env vars are set 
if [ -z "${MIN_SIGNERS}" ]; then
    MIN_SIGNERS=1
fi

if [ -z "${MAX_SIGNERS}" ]; then
    echo "Missing environment variable: MAX_SIGNERS" >/dev/stderr
    exit 1
fi

if [ -z "${STEP_SIGNERS}" ]; then
    echo "Missing environment variable: STEP_SIGNERS" >/dev/stderr
    exit 1
fi

if [ -z "${MIN_CLIENTS}" ]; then
    MIN_CLIENTS=0
fi

if [ -z "${MAX_CLIENTS}" ]; then
    echo "Missing environment variable: MAX_CLIENTS" >/dev/stderr
    exit 1
fi

if [ -z "${STEP_CLIENTS}" ]; then
    echo "Missing environment variable: STEP_CLIENTS" >/dev/stderr
    exit 1
fi

if [ -z "${AGGREGATOR_DIR}" ]; then
    AGGREGATOR_DIR=../../target/release
    echo "Using the default AGGREGATOR_DIR: $AGGREGATOR_DIR"
fi

if [ -z "${OUT_FILE}" ]; then
    OUT_FILE="benchmark/benchmark-data-[$MIN_SIGNERS,$MAX_SIGNERS;$STEP_SIGNERS]Sx[$MIN_CLIENTS,$MAX_CLIENTS;$STEP_CLIENTS]C-$(date +%Y-%m-%d-%H-%M-%S).tsv"
    echo "Using the default OUT_FILE: $OUT_FILE"
fi
mkdir -p $(dirname "$OUT_FILE")
OUT_FILE_TMP=${OUT_FILE}.tmp

# Run stress test
RUN_STRESS_TEST() {
    NUM_SIGNERS=$1
    NUM_SIGNERS=$(( NUM_SIGNERS > 0 ? NUM_SIGNERS : 1 ))
    NUM_CLIENTS=$2
    OUT_FILE=$3
    INDEX_RUN=$4
    TOTAL_RUN=$5
    echo ">> [#$INDEX_RUN/$TOTAL_RUN] Running stress test with $NUM_SIGNERS signers and $NUM_CLIENTS clients"
    COMMAND="./load-aggregator --cardano-cli-path ./script/mock-cardano-cli --aggregator-dir $AGGREGATOR_DIR --num-signers=$NUM_SIGNERS --num-clients=$NUM_CLIENTS"
    if $($COMMAND >> $OUT_FILE_TMP) ; then
        echo ">>>> Success"
    else
        echo "signers	clients	phase	duration/ms" >> $OUT_FILE_TMP
        echo "$NUM_SIGNERS	$NUM_CLIENTS	failure	10" >> $OUT_FILE_TMP
        echo ">>>> Failure"
    fi
    if [[ $INDEX_RUN -gt 1 ]] ; then
        tail -n +2 $OUT_FILE_TMP >> $OUT_FILE
    else
        cat $OUT_FILE_TMP >> $OUT_FILE
    fi
    rm -f $OUT_FILE_TMP
    echo ""
}

# Run aggregator benchmark over a range of signers and clients
SIGNERS_RANGE=$(seq -s ' ' $MIN_SIGNERS $STEP_SIGNERS $MAX_SIGNERS)
CLIENTS_RANGE=$(seq -s ' ' $MIN_CLIENTS $STEP_CLIENTS $MAX_CLIENTS)
SIGNERS_RANGE_LENGTH=$(echo $SIGNERS_RANGE | grep -o " " | wc -l)
CLIENTS_RANGE_LENGTH=$(echo $CLIENTS_RANGE | grep -o " " | wc -l)
echo ""
echo "Run aggregator benchmark over the range:"
echo ">> Signers: [$SIGNERS_RANGE]"
echo ">> Clients: [$CLIENTS_RANGE]"
echo ""

INDEX_RUN=1
TOTAL_RUN=$(( $SIGNERS_RANGE_LENGTH * CLIENTS_RANGE_LENGTH ))
for NUM_SIGNERS in $SIGNERS_RANGE; do 
    for NUM_CLIENTS in $CLIENTS_RANGE; do
        RUN_STRESS_TEST $NUM_SIGNERS $NUM_CLIENTS $OUT_FILE $INDEX_RUN $TOTAL_RUN
        INDEX_RUN=$(( ${INDEX_RUN} + 1))
    done 
done 

# Clean directory
rm -f $OUT_FILE_TMP

