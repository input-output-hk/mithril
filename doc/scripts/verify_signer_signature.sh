#!/bin/bash

set -e

if [ -z "$AGGREGATOR_ENDPOINT" ] || [ -z "$PARTY_ID" ]; then
    echo ">> ERROR: Required environment variables AGGREGATOR_ENDPOINT and/or PARTY_ID are not set."
    exit 1
fi

CERTIFICATES_RESPONSE=$(curl -s "$AGGREGATOR_ENDPOINT/certificates" -H 'accept: application/json')
CERTIFICATES_COUNT=$(echo "$CERTIFICATES_RESPONSE" | jq '. | length')

echo "$CERTIFICATES_RESPONSE" | jq -r '.[] | .hash' | while read -r HASH; do
    RESPONSE=$(curl -s "$AGGREGATOR_ENDPOINT/certificate/$HASH" -H 'accept: application/json')
    SIGNER_COUNT=$(echo "$RESPONSE" | jq '.metadata.signers | length')
    for (( i=0; i < SIGNER_COUNT; i++ )); do
        PARTY_ID_RESPONSE=$(echo "$RESPONSE" | jq -r ".metadata.signers[$i].party_id")
        if [[ "$PARTY_ID_RESPONSE" == "$PARTY_ID" ]]; then
            echo ">> Congrats, you have signed this certificate: $AGGREGATOR_ENDPOINT/certificate/$HASH !"
            exit 1
        fi
    done
done

echo ">> Oops, your party id was not found in the last ${CERTIFICATES_COUNT} certificates. Please try again later."
