#!/usr/bin/env bash
# Cardano transactions prover benchmark: runs it, extracts previous results from an issue, renders the results comment.
# Only `run` sends requests (to the aggregator under test), the other modes are read-only and print the comment command.
# Rights: none for run and previous (public aggregator and issue); the printed comment needs an authenticated account.
# Requirements: awk, sed, ab (run), gh (previous), join and sort (comment).
# Exit codes: 0 done, 1 benchmark failure or a row below the tolerance, 2 usage or network error.
# Usage:
#   bash prover-benchmark.sh run --aggregator <endpoint> --benchmark-dir <mithril-test-lab/benchmark/aggregator-prover>
#                                --transactions-file <file> --work-dir <dir>
#   bash prover-benchmark.sh previous --issue <number> --out <csv>
#   bash prover-benchmark.sh comment --work-dir <dir> --previous <csv> --machine-type <type> --vcpus <n> --ram-gb <n>
#                                    --issue <number> [--tolerance <percent>]
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands awk sed

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
MODE="${1:-}"
shift || true
AGGREGATOR="" BENCHMARK_DIR="" TRANSACTIONS_FILE="" WORK_DIR="" ISSUE="" OUT="" PREVIOUS="" MACHINE_TYPE="" VCPUS="" RAM_GB="" TOLERANCE=20
TRANSACTIONS_PER_REQUEST_MIN=0 TRANSACTIONS_PER_REQUEST_MAX=50 TRANSACTIONS_PER_REQUEST_STEP=10
AB_TOTAL_REQUESTS=500 AB_CONCURRENCY_MIN=50 AB_CONCURRENCY_MAX=50 AB_CONCURRENCY_STEP=50
while [[ $# -gt 0 ]]; do
  case "$1" in
    --aggregator) AGGREGATOR="${2:?}"; shift 2 ;;
    --benchmark-dir) BENCHMARK_DIR="${2:?}"; shift 2 ;;
    --transactions-file) TRANSACTIONS_FILE="${2:?}"; shift 2 ;;
    --work-dir) WORK_DIR="${2:?}"; shift 2 ;;
    --issue) ISSUE="${2:?}"; shift 2 ;;
    --out) OUT="${2:?}"; shift 2 ;;
    --previous) PREVIOUS="${2:?}"; shift 2 ;;
    --machine-type) MACHINE_TYPE="${2:?}"; shift 2 ;;
    --vcpus) VCPUS="${2:?}"; shift 2 ;;
    --ram-gb) RAM_GB="${2:?}"; shift 2 ;;
    --tolerance) TOLERANCE="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
usage() { usage_error; }
parameter() { sed -n "s/^$1=//p" "$WORK_DIR/benchmark.env"; }

table() {
  echo "| **total_requests** | **concurrency** | **transactions/request** | **requests/s** |"
  echo "|--------------------|-----------------|--------------------------|----------------|"
  awk -F, 'NR > 1 { printf "| %-18s | %-15s | %-24s | %-14s |\n", $1, $2, $3, $4 }' "$1"
}

case "$MODE" in
  run)
    [[ -n "$AGGREGATOR" && -d "$BENCHMARK_DIR" && -f "$TRANSACTIONS_FILE" && -n "$WORK_DIR" ]] || usage
    require_commands ab
    WORK_DIR="$(mkdir -p "$WORK_DIR" && cd "$WORK_DIR" && pwd)" || exit 2
    BENCHMARK_DIR="$(cd "$BENCHMARK_DIR" && pwd)" || exit 2
    TRANSACTIONS_FILE="$(cd "$(dirname "$TRANSACTIONS_FILE")" && pwd)/$(basename "$TRANSACTIONS_FILE")" || exit 2
    cd "$WORK_DIR" || exit 2
    printf 'AGGREGATOR_ENDPOINT=%s\nTRANSACTIONS_FILE=%s\n' "$AGGREGATOR" "$(basename "$TRANSACTIONS_FILE")" >"$WORK_DIR/benchmark.env"
    export AGGREGATOR_ENDPOINT="$AGGREGATOR" TRANSACTIONS_FILE OUT_FILE="$WORK_DIR/benchmark.csv"
    export TRANSACTIONS_PER_REQUEST_MIN TRANSACTIONS_PER_REQUEST_MAX TRANSACTIONS_PER_REQUEST_STEP
    export AB_TOTAL_REQUESTS AB_CONCURRENCY_MIN AB_CONCURRENCY_MAX AB_CONCURRENCY_STEP
    bash "$BENCHMARK_DIR/benchmark-aggregator-prover.sh" 2>&1 \
      | sed -e "s|$TRANSACTIONS_FILE|$(basename "$TRANSACTIONS_FILE")|" -e "s|$WORK_DIR/||" | tee "$WORK_DIR/benchmark.log"
    STATUS=${PIPESTATUS[0]}
    grep -q '>>>> Failure' "$WORK_DIR/benchmark.log" && STATUS=1
    exit "$STATUS"
    ;;
  previous)
    [[ -n "$ISSUE" && -n "$OUT" ]] || usage
    require_commands gh
    gh issue view "$ISSUE" --repo "$REPO" --comments --json comments --jq '.comments[].body' \
      | awk '/^## Cardano transactions prover benchmark/ { found = 1 } found && /^vs previous results/ { exit } found' \
      | awk -F'|' 'BEGIN { print "total_requests,concurrency,transactions/request,requests/s" }
          NF >= 5 && $2 ~ /[0-9]/ { gsub(/ /, ""); print $2 "," $3 "," $4 "," $5 }' >"$OUT"
    [[ $(wc -l <"$OUT") -gt 1 ]] || { echo "No benchmark results in the comments of #$ISSUE" >&2; exit 2; }
    cat "$OUT"
    ;;
  comment)
    require_commands join sort
    [[ -f "$WORK_DIR/benchmark.csv" && -f "$WORK_DIR/benchmark.log" && -f "$WORK_DIR/benchmark.env" && -f "$PREVIOUS" && -n "$MACHINE_TYPE" && -n "$VCPUS" && -n "$RAM_GB" && -n "$ISSUE" ]] || usage
    RESULTS="$WORK_DIR/benchmark.csv" COMMENT="$WORK_DIR/benchmark-comment.md"
    {
      echo "## Cardano transactions prover benchmark"
      echo
      echo "> [!NOTE]"
      echo "> run on GCP with \`$MACHINE_TYPE\` machine with \`$VCPUS\` vCPUs and \`${RAM_GB}GB\` of RAM"
      echo "> not during Cardano database snapshotting"
      echo
      echo "- Benchmark results:"
      echo
      table "$RESULTS"
      echo
      echo "vs previous results"
      echo
      table "$PREVIOUS"
      echo
      echo "- Command run:"
      echo '```bash'
      cat <<EOF
# Aggregator endpoint
export AGGREGATOR_ENDPOINT=$(parameter AGGREGATOR_ENDPOINT)

# Transactions file to prove
export TRANSACTIONS_FILE=$(parameter TRANSACTIONS_FILE)

# Transactions proved per request range definition
export TRANSACTIONS_PER_REQUEST_MIN=$TRANSACTIONS_PER_REQUEST_MIN
export TRANSACTIONS_PER_REQUEST_MAX=$TRANSACTIONS_PER_REQUEST_MAX
export TRANSACTIONS_PER_REQUEST_STEP=$TRANSACTIONS_PER_REQUEST_STEP

# Apache benchmark total request sent per benchmark
export AB_TOTAL_REQUESTS=$AB_TOTAL_REQUESTS

# Apache benchmark concurrency level range definition
export AB_CONCURRENCY_MIN=$AB_CONCURRENCY_MIN
export AB_CONCURRENCY_MAX=$AB_CONCURRENCY_MAX
export AB_CONCURRENCY_STEP=$AB_CONCURRENCY_STEP

# Run the benchmark:
./benchmark-aggregator-prover.sh
EOF
      echo '```'
      echo
      echo "- Command result:"
      echo '```bash'
      sed -E -e 's|/[^ ]*/(transactions-[a-z]+\.txt)|\1|' -e "s|$WORK_DIR/||" -e '/^exit /d' "$WORK_DIR/benchmark.log"
      echo '```'
    } >"$COMMENT"
    echo "| tx/request | previous | current | delta | |"
    echo "|---|---|---|---|---|"
    ROWS="$(join -t, <(awk -F, 'NR > 1 { print $3 "," $4 }' "$PREVIOUS" | sort -t, -k1,1) \
      <(awk -F, 'NR > 1 { print $3 "," $4 }' "$RESULTS" | sort -t, -k1,1) | sort -t, -n -k1,1 \
      | awk -F, -v t="$TOLERANCE" '{ d = ($3 - $2) * 100 / $2; printf "| %s | %s | %s | %+.1f %% | %s |\n", $1, $2, $3, d, (d < -t) ? "🔴" : "🟢" }')"
    echo "$ROWS"
    FLAGGED="$(grep -c '🔴' <<<"$ROWS")"
    echo
    echo "Rows more than $TOLERANCE % below the previous results: $FLAGGED"
    echo "Comment: $COMMENT"
    echo
    echo "gh issue comment $ISSUE --repo $REPO --body-file $COMMENT"
    [[ "$FLAGGED" == 0 ]]
    ;;
  *) usage ;;
esac
