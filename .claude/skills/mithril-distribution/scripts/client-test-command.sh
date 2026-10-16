#!/usr/bin/env bash
# Renders the dispatch command of the client multi-platform test, with transactions and blocks hashes certified by the aggregator.
# Read-only: queries the aggregator proof routes and the workflow definition, prints the command.
# Exit codes: 0 command rendered, 1 not enough certified hashes or unknown workflow input, 2 usage or network error.
# Usage:
#   bash client-test-command.sh --commit <sha> --image-id <image id> --network <cardano network> --aggregator <endpoint>
#                               --genesis-key-url <url> [--ancillary-key-url <url>] --transactions-file <file> --blocks-file <file>
#                               [--count <n>]
set -uo pipefail

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
COMMIT="" IMAGE_ID="" NETWORK="" AGGREGATOR="" GENESIS_KEY_URL="" ANCILLARY_KEY_URL="" TRANSACTIONS_FILE="" BLOCKS_FILE="" COUNT=3
while [[ $# -gt 0 ]]; do
  case "$1" in
    --commit) COMMIT="${2:?}"; shift 2 ;;
    --image-id) IMAGE_ID="${2:?}"; shift 2 ;;
    --network) NETWORK="${2:?}"; shift 2 ;;
    --aggregator) AGGREGATOR="${2:?}"; shift 2 ;;
    --genesis-key-url) GENESIS_KEY_URL="${2:?}"; shift 2 ;;
    --ancillary-key-url) ANCILLARY_KEY_URL="${2:?}"; shift 2 ;;
    --transactions-file) TRANSACTIONS_FILE="${2:?}"; shift 2 ;;
    --blocks-file) BLOCKS_FILE="${2:?}"; shift 2 ;;
    --count) COUNT="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
for value in "$COMMIT" "$IMAGE_ID" "$NETWORK" "$AGGREGATOR" "$GENESIS_KEY_URL" "$TRANSACTIONS_FILE" "$BLOCKS_FILE"; do
  [[ -n "$value" ]] || { sed -n '5,8p' "$0" >&2; exit 2; }
done

certified_hashes() {
  local file="$1" route="$2" parameter="$3" uncertified_field="$4" found=()
  while read -r hash && [[ ${#found[@]} -lt $COUNT ]]; do
    [[ -n "$hash" ]] || continue
    local response
    response="$(curl -sf -m 30 "$AGGREGATOR$route?$parameter=$hash")" || continue
    [[ "$(jq -r --arg h "$hash" ".$uncertified_field | index(\$h) // empty" <<<"$response")" == "" ]] && found+=("$hash")
  done <"$file"
  [[ ${#found[@]} -eq $COUNT ]] || { echo "Only ${#found[@]} of $COUNT hashes of $file are certified by $AGGREGATOR" >&2; return 1; }
  (IFS=,; echo "${found[*]}")
}

TRANSACTIONS="$(certified_hashes "$TRANSACTIONS_FILE" /proof/cardano-transaction transaction_hashes non_certified_transactions)" || exit 1
BLOCKS="$(certified_hashes "$BLOCKS_FILE" /proof/v2/cardano-block block_hashes non_certified_blocks)" || exit 1
echo "Certified: $COUNT transactions, $COUNT blocks"

INPUTS="$(gh workflow view test-client.yml --repo "$REPO" --yaml \
  | awk '/^  workflow_dispatch:/{f=1; next} f && /^  [a-z_]+:/{exit} f && /^      [a-z_]+:$/{gsub(/[ :]/, ""); print}')" \
  || { echo "Cannot read test-client.yml" >&2; exit 2; }
for input in commit_sha docker_image_id network aggregator_endpoint genesis_verification_key transactions_hashes_to_certify blocks_hashes_to_certify; do
  grep -qx "$input" <<<"$INPUTS" || { echo "Input $input not accepted by the workflow_dispatch of test-client.yml" >&2; exit 1; }
done

echo
echo "gh workflow run test-client.yml --repo $REPO -f commit_sha=$COMMIT -f docker_image_id=$IMAGE_ID -f network=$NETWORK -f aggregator_endpoint=$AGGREGATOR -f genesis_verification_key=$GENESIS_KEY_URL -f transactions_hashes_to_certify=$TRANSACTIONS -f blocks_hashes_to_certify=$BLOCKS${ANCILLARY_KEY_URL:+ -f ancillary_verification_key=$ANCILLARY_KEY_URL}"
