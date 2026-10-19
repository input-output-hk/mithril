#!/usr/bin/env bash
# Read-only health check of a Mithril network aggregator, printed as a Markdown table.
# Rights: none (public aggregator routes, networks.json and Koios).
# Exit codes: 0 all checks pass, 1 at least one check fails, 2 usage or network error,
#             3 no failure but the --wait-target epoch is not reached, or neither CardanoTransactions nor CardanoBlocksTransactions has an artifact
#               created after --await-transactions-after yet (the other types are checked with --epochs after 2 epochs).
# Baseline: --baseline fails when the next signers or the next stake drop by more than 10 % from the saved baseline.
# Requirements: curl, jq, awk, sort, GNU date.
# Usage:
#   bash network-check.sh <network> [--expect-commit <sha>] [--expect-types <A,B>] [--require-artifacts-after <ISO>] [--await-transactions-after <ISO>]
#                                   [--save-baseline <file>] [--baseline <file>]
#   bash network-check.sh <network> --epochs <from> <to> [--baseline <file>]
#   bash network-check.sh <network> --wait-target <deployment epoch> <epochs to wait>
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2

usage() {
  script_help >&2
  exit 2
}
handle_help "$@"
require_commands curl jq awk sort
require_gnu_date

NETWORK="${1:-}"
[[ -n "$NETWORK" && "$NETWORK" != -* ]] || usage
shift

EXPECT_COMMIT="" EXPECT_TYPES="" REQUIRE_ARTIFACTS_AFTER="" AWAIT_TRANSACTIONS_AFTER="" SAVE_BASELINE="" BASELINE="" EPOCH_FROM="" EPOCH_TO="" DEPLOYMENT_EPOCH="" EPOCHS_TO_WAIT=""
MAXIMUM_DROP_PERCENT=10
while [[ $# -gt 0 ]]; do
  case "$1" in
    --expect-commit) EXPECT_COMMIT="${2:?}"; shift 2 ;;
    --expect-types) EXPECT_TYPES="${2:?}"; shift 2 ;;
    --require-artifacts-after) REQUIRE_ARTIFACTS_AFTER="${2:?}"; shift 2 ;;
    --await-transactions-after) AWAIT_TRANSACTIONS_AFTER="${2:?}"; shift 2 ;;
    --save-baseline) SAVE_BASELINE="${2:?}"; shift 2 ;;
    --baseline) BASELINE="${2:?}"; shift 2 ;;
    --epochs) EPOCH_FROM="${2:?}"; EPOCH_TO="${3:?}"; shift 3 ;;
    --wait-target) DEPLOYMENT_EPOCH="${2:?}"; EPOCHS_TO_WAIT="${3:?}"; shift 3 ;;
    *) echo "Unknown option $1" >&2; usage ;;
  esac
done

AGGREGATOR="https://aggregator.$NETWORK.api.mithril.network/aggregator"
FAILURES=0
PENDINGS=0

fetch() { curl -sf -m 30 "$AGGREGATOR$1"; }
row() {
  local check="$1" value="$2" result="$3"
  [[ "$result" == "fail" ]] && FAILURES=$((FAILURES + 1))
  [[ "$result" == pending* ]] && PENDINGS=$((PENDINGS + 1))
  printf '| %s | %s | %s |\n' "$check" "$value" "$(result_marker "$result")"
}
result_marker() {
  case "$1" in
    pass) echo "🟢 pass" ;;
    fail) echo "🔴 fail" ;;
    pending*) echo "🟡 $1" ;;
    *) echo "⚪ $1" ;;
  esac
}
artifact_route() {
  case "$1" in
    MithrilStakeDistribution) echo mithril-stake-distributions ;;
    CardanoStakeDistribution) echo cardano-stake-distributions ;;
    CardanoDatabase) echo cardano-database ;;
    CardanoTransactions) echo cardano-transactions ;;
    CardanoBlocksTransactions) echo cardano-blocks-transactions ;;
    *) echo "" ;;
  esac
}
to_seconds() { date -u -d "$1" +%s; }
drop_result() {
  local current="$1" reference="$2"
  awk -v c="$current" -v r="$reference" -v m="$MAXIMUM_DROP_PERCENT" 'BEGIN { print (r > 0 && (r - c) * 100 / r > m) ? "fail" : "pass" }'
}

STATUS="$(fetch /status)" || { echo "Aggregator of $NETWORK not answering at $AGGREGATOR" >&2; exit 2; }
ROOT="$(fetch /)" || { echo "Aggregator root route of $NETWORK not answering" >&2; exit 2; }
EPOCH="$(jq -r .epoch <<<"$STATUS")"
TYPES="$(jq -r '.capabilities.signed_entity_types | join(",")' <<<"$ROOT")"

if [[ -n "$DEPLOYMENT_EPOCH" ]]; then
  CARDANO_NETWORK="$(jq -r .cardano_network <<<"$STATUS")"
  case "$CARDANO_NETWORK" in
    preview) KOIOS=https://preview.koios.rest EPOCH_SECONDS=86400 ;;
    preprod) KOIOS=https://preprod.koios.rest EPOCH_SECONDS=432000 ;;
    mainnet) KOIOS=https://api.koios.rest EPOCH_SECONDS=432000 ;;
    *) echo "Unknown Cardano network $CARDANO_NETWORK" >&2; exit 2 ;;
  esac
  START="$(curl -sf -m 30 "$KOIOS/api/v1/epoch_info?_epoch_no=$EPOCH&select=start_time" | jq -r '.[0].start_time // empty')"
  [[ -n "$START" ]] || { echo "Cannot read the start of epoch $EPOCH from $KOIOS" >&2; exit 2; }
  TARGET=$((DEPLOYMENT_EPOCH + EPOCHS_TO_WAIT))
  TARGET_START="$(date -u -d "@$((START + (TARGET - EPOCH) * EPOCH_SECONDS))" +%Y-%m-%dT%H:%M:%SZ)"
  echo "\`$NETWORK\`: current epoch $EPOCH, deployed in epoch $DEPLOYMENT_EPOCH, target epoch $TARGET starts $TARGET_START"
  echo "Check on resume: bash $0 $NETWORK --epochs $((DEPLOYMENT_EPOCH + 1)) $TARGET --baseline <baseline file>"
  [[ $EPOCH -ge $TARGET ]] || exit 3
  exit 0
fi

echo "### \`$NETWORK\` at epoch $EPOCH ($(date -u +%Y-%m-%dT%H:%M:%SZ))"
echo
echo "| Check | Value | Result |"
echo "|---|---|---|"

if [[ -n "$EPOCH_FROM" ]]; then
  MSD="$(fetch /artifact/mithril-stake-distributions)" || { echo "Cannot list stake distributions" >&2; exit 2; }
  REFERENCE_SIGNERS="$( [[ -n "$BASELINE" ]] && jq -r .total_signers "$BASELINE" || echo 0)"
  for ((epoch = EPOCH_FROM; epoch <= EPOCH_TO; epoch++)); do
    DISTRIBUTION="$(jq -c --argjson e "$epoch" '[.[] | select(.epoch == $e)] | first // empty' <<<"$MSD")"
    if [[ -z "$DISTRIBUTION" ]]; then
      row "Epoch $epoch registered signers" "no Mithril stake distribution" fail
      continue
    fi
    REGISTERED="$(fetch "/artifact/mithril-stake-distribution/$(jq -r .hash <<<"$DISTRIBUTION")" | jq -r '.signers | length')"
    row "Epoch $epoch registered signers" "$REGISTERED (baseline $REFERENCE_SIGNERS)" "$(drop_result "$REGISTERED" "$REFERENCE_SIGNERS")"
    SIGNED="$(fetch "/certificate/$(jq -r .certificate_hash <<<"$DISTRIBUTION")" | jq -r '.metadata.signers | length')"
    row "Epoch $epoch signers of the stake distribution certificate" "$SIGNED of $REGISTERED" info
    for TYPE in ${TYPES//,/ }; do
      ROUTE="$(artifact_route "$TYPE")"
      [[ -n "$ROUTE" ]] || continue
      if [[ "$TYPE" == "CardanoDatabase" ]]; then
        ITEMS="$(fetch "/artifact/cardano-database/epoch/$epoch" | jq -c '[.[] | {created_at}]')"
      else
        OFFSET="$([[ "$TYPE" == "CardanoStakeDistribution" ]] && echo 1 || echo 0)"
        ITEMS="$(fetch "/artifact/$ROUTE" | jq -c --argjson e "$epoch" --argjson o "$OFFSET" '[.[] | select(.epoch + $o == $e) | {created_at}]')"
      fi
      COUNT="$(jq length <<<"${ITEMS:-[]}")"
      if [[ "$COUNT" -gt 0 ]]; then
        row "Epoch $epoch \`$TYPE\` certified" "$COUNT in the latest artifacts" pass
      else
        OLDEST="$(fetch "/artifact/$ROUTE" | jq -r '[.[] | (.epoch // .beacon.epoch)] | min')"
        if [[ "$OLDEST" -gt "$epoch" ]]; then
          row "Epoch $epoch \`$TYPE\` certified" "older than the 20 latest artifacts (oldest epoch $OLDEST)" "n/a"
        else
          row "Epoch $epoch \`$TYPE\` certified" "none" fail
        fi
      fi
    done
  done
  echo
  echo "Failures: $FAILURES"
  [[ $FAILURES -eq 0 ]]
  exit $?
fi

VERSION="$(jq -r .aggregator_node_version <<<"$STATUS")"
if [[ -n "$EXPECT_COMMIT" ]]; then
  [[ "${VERSION#*+}" == "${EXPECT_COMMIT:0:7}" ]] && row "Deployed version" "\`$VERSION\`" pass || row "Deployed version" "\`$VERSION\`, expected \`+${EXPECT_COMMIT:0:7}\`" fail
else
  row "Deployed version" "\`$VERSION\`" info
fi

CARDANO_VERSION="$(jq -r .cardano_node_version <<<"$STATUS")"
CARDANO_NETWORK="$(jq -r .cardano_network <<<"$STATUS")"
MINIMUM="$(curl -sf -m 30 https://raw.githubusercontent.com/IntersectMBO/mithril/main/networks.json \
  | jq -r --arg n "$CARDANO_NETWORK" '.[$n]["cardano-minimum-version"]["mithril-aggregator"] // empty')"
if [[ -n "$MINIMUM" ]]; then
  [[ "$(printf '%s\n%s\n' "$MINIMUM" "$CARDANO_VERSION" | sort -V | head -1)" == "$MINIMUM" ]] \
    && row "Cardano node" "\`$CARDANO_VERSION\` (minimum \`$MINIMUM\`)" pass \
    || row "Cardano node" "\`$CARDANO_VERSION\` (minimum \`$MINIMUM\`)" fail
else
  row "Cardano node" "\`$CARDANO_VERSION\` (no minimum found)" info
fi

if [[ -n "$EXPECT_TYPES" ]]; then
  [[ "$(tr ',' '\n' <<<"$TYPES" | sort | paste -sd,)" == "$(tr ',' '\n' <<<"$EXPECT_TYPES" | sort | paste -sd,)" ]] \
    && row "Signed entity types" "$TYPES" pass || row "Signed entity types" "$TYPES, expected $EXPECT_TYPES" fail
else
  row "Signed entity types" "$TYPES" info
fi

for ((epoch = EPOCH; epoch <= EPOCH + 1; epoch++)); do
  CONFIGURATION="$(fetch "/protocol-configuration/$epoch")"
  if [[ -n "$CONFIGURATION" ]]; then
    row "Protocol configuration epoch $epoch" "\`$(jq -c '{p: .protocol_parameters, tx: .cardano_transactions_signing_config, btx: .cardano_blocks_transactions_signing_config, types: .available_signed_entity_types}' <<<"$CONFIGURATION")\`" pass
  else
    row "Protocol configuration epoch $epoch" "not served" fail
  fi
done

NOW="$(date -u +%s)"
DEPLOYED_ARTIFACTS="" WAITING_ARTIFACTS=""
for TYPE in ${TYPES//,/ }; do
  ROUTE="$(artifact_route "$TYPE")"
  [[ -n "$ROUTE" ]] || { row "\`$TYPE\` latest certificate" "no artifact route known" info; continue; }
  LATEST="$(fetch "/artifact/$ROUTE" | jq -c 'first // empty | {epoch: (.epoch // .beacon.epoch), created_at}')"
  if [[ -z "$LATEST" ]]; then
    row "\`$TYPE\` latest certificate" "none" fail
    continue
  fi
  LATEST_EPOCH="$(jq -r .epoch <<<"$LATEST")"
  CREATED_AT="$(jq -r .created_at <<<"$LATEST")"
  AGE_HOURS="$(( (NOW - $(to_seconds "$CREATED_AT")) / 3600 ))"
  RESULT="$([[ "$LATEST_EPOCH" -ge $((EPOCH - 1)) ]] && echo pass || echo fail)"
  if [[ -n "$REQUIRE_ARTIFACTS_AFTER" && "$TYPE" != "MithrilStakeDistribution" && "$TYPE" != "CardanoStakeDistribution" && "$CREATED_AT" < "$REQUIRE_ARTIFACTS_AFTER" ]]; then
    RESULT="fail"
  fi
  row "\`$TYPE\` latest certificate" "epoch $LATEST_EPOCH, $AGE_HOURS h ago" "$RESULT"
  if [[ -n "$AWAIT_TRANSACTIONS_AFTER" && ( "$TYPE" == "CardanoTransactions" || "$TYPE" == "CardanoBlocksTransactions" ) ]]; then
    if [[ "$(to_seconds "$CREATED_AT")" -gt "$(to_seconds "$AWAIT_TRANSACTIONS_AFTER")" ]]; then
      DEPLOYED_ARTIFACTS="$DEPLOYED_ARTIFACTS \`$TYPE\` $CREATED_AT"
    else
      WAITING_ARTIFACTS="$WAITING_ARTIFACTS \`$TYPE\` latest $CREATED_AT"
    fi
  fi
done

if [[ -n "$AWAIT_TRANSACTIONS_AFTER" ]]; then
  if [[ -n "$DEPLOYED_ARTIFACTS" ]]; then
    row "Transactions artifact created after the deployment ($AWAIT_TRANSACTIONS_AFTER)" "${DEPLOYED_ARTIFACTS# }" pass
  else
    row "Transactions artifact created after the deployment ($AWAIT_TRANSACTIONS_AFTER)" "${WAITING_ARTIFACTS:- no transactions signed entity type}" pending
  fi
fi

if [[ -n "$REQUIRE_ARTIFACTS_AFTER" ]]; then
  SNAPSHOT="$(fetch /artifact/cardano-database | jq -r --arg s "$REQUIRE_ARTIFACTS_AFTER" '[.[] | select(.created_at > $s)] | first | .hash // empty')"
  if [[ -n "$SNAPSHOT" ]]; then
    ANCILLARY="$(fetch "/artifact/cardano-database/$SNAPSHOT" | jq -r '.ancillary.locations | length')"
    [[ "$ANCILLARY" -gt 0 ]] && row "Snapshot with ancillary files created after $REQUIRE_ARTIFACTS_AFTER" "\`${SNAPSHOT:0:12}\`" pass \
      || row "Snapshot with ancillary files created after $REQUIRE_ARTIFACTS_AFTER" "\`${SNAPSHOT:0:12}\` has no ancillary location" fail
  else
    row "Snapshot with ancillary files created after $REQUIRE_ARTIFACTS_AFTER" "no snapshot yet" fail
  fi
fi

SIGNERS="$(jq -r .total_signers <<<"$STATUS")"
NEXT_SIGNERS="$(jq -r .total_next_signers <<<"$STATUS")"
STAKE="$(jq -r .total_stakes_signers <<<"$STATUS")"
NEXT_STAKE="$(jq -r .total_next_stakes_signers <<<"$STATUS")"
if [[ -n "$BASELINE" ]]; then
  row "Signers current / next" "$SIGNERS / $NEXT_SIGNERS (baseline $(jq -r '"\(.total_signers) / \(.total_next_signers)"' "$BASELINE"))" \
    "$(drop_result "$NEXT_SIGNERS" "$(jq -r .total_next_signers "$BASELINE")")"
  row "Stake current / next" "$STAKE / $NEXT_STAKE" "$(drop_result "$NEXT_STAKE" "$(jq -r .total_next_stakes_signers "$BASELINE")")"
else
  row "Signers current / next" "$SIGNERS / $NEXT_SIGNERS" info
  row "Stake current / next" "$STAKE / $NEXT_STAKE" info
fi

if [[ -n "$SAVE_BASELINE" ]]; then
  mkdir -p "$(dirname "$SAVE_BASELINE")"
  jq --arg at "$(date -u +%Y-%m-%dT%H:%M:%SZ)" --arg types "$TYPES" '. + {saved_at: $at, signed_entity_types: $types}' <<<"$STATUS" >"$SAVE_BASELINE"
  echo
  echo "Baseline saved to $SAVE_BASELINE"
fi

echo
echo "Failures: $FAILURES"
[[ -n "$AWAIT_TRANSACTIONS_AFTER" ]] && echo "Pending: $PENDINGS"
[[ $FAILURES -eq 0 ]] || exit 1
[[ $PENDINGS -eq 0 ]] || exit 3
