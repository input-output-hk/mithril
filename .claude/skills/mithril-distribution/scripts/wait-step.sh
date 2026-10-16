#!/usr/bin/env bash
# Polls a detect-step.sh probe until it is done, failed or the timeout is reached.
# Exit codes: those of detect-step.sh (0 done, 2 probe error, 3 failed), 4 timeout.
# Usage: bash wait-step.sh [--interval <seconds>] [--timeout <minutes>] <probe> <args...>
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INTERVAL=30
TIMEOUT_MINUTES=30
while [[ $# -gt 0 ]]; do
  case "$1" in
    --interval) INTERVAL="${2:?}"; shift 2 ;;
    --timeout) TIMEOUT_MINUTES="${2:?}"; shift 2 ;;
    *) break ;;
  esac
done
[[ $# -ge 1 ]] || { sed -n '4p' "$0" >&2; exit 2; }

DEADLINE=$(( $(date +%s) + TIMEOUT_MINUTES * 60 ))
while true; do
  OUTPUT="$(bash "$SCRIPT_DIR/detect-step.sh" "$@" 2>&1)"
  STATUS=$?
  echo "$(date -u +%H:%M:%S) $OUTPUT"
  [[ $STATUS -ne 1 ]] && exit $STATUS
  [[ $(date +%s) -ge $DEADLINE ]] && { echo "TIMEOUT after $TIMEOUT_MINUTES min"; exit 4; }
  sleep "$INTERVAL"
done
