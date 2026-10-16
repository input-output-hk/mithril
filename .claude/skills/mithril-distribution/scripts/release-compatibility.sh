#!/usr/bin/env bash
# Renders the release notes with the networks marked compatible and the Cardano and DMQ node compatibility tables
# regenerated from a networks.json file with the repository generators, the rest of the body unchanged.
# Read-only: writes the notes to a local file and prints the edit command.
# Rights: public reads; the printed release edit needs repository Write role (Contents write).
# Requirements: gh, jq, awk, sed, diff.
# Exit codes: 0 rendered, 1 nothing to change, 2 usage or GitHub error.
# Usage:
#   bash release-compatibility.sh --tag <tag> --compatible <network>[,<network>...] --networks-json <file>
#                                 --repository <mithril checkout> --out <file>
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands gh jq awk sed diff

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
TAG="" COMPATIBLE="" NETWORKS_JSON="" REPOSITORY="" OUT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --tag) TAG="${2:?}"; shift 2 ;;
    --compatible) COMPATIBLE="${2:?}"; shift 2 ;;
    --networks-json) NETWORKS_JSON="${2:?}"; shift 2 ;;
    --repository) REPOSITORY="${2:?}"; shift 2 ;;
    --out) OUT="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
GENERATORS="$REPOSITORY/.github/workflows/scripts"
[[ -n "$TAG" && -n "$COMPATIBLE" && -f "$NETWORKS_JSON" && -f "$GENERATORS/compute-cardano-compatibility.sh" && -n "$OUT" ]] \
  || usage_error

BODY="$(gh release view "$TAG" --repo "$REPO" --json body --jq .body)" || { echo "No release $TAG" >&2; exit 2; }
CARDANO="$(bash "$GENERATORS/compute-cardano-compatibility.sh" "$NETWORKS_JSON")" || exit 2
DMQ=""
[[ -f "$GENERATORS/compute-dmq-compatibility.sh" ]] && { DMQ="$(bash "$GENERATORS/compute-dmq-compatibility.sh" "$NETWORKS_JSON")" || exit 2; }

replace_section() {
  local heading="$1" replacement="$2"
  awk -v heading="$heading" -v replacement="$replacement" '
    index($0, heading) == 1 { print substr(replacement, index(replacement, heading)); skipping = 1; next }
    skipping && /^## / { skipping = 0; print "" }
    !skipping { print }
  '
}

NEW_BODY="$BODY"
for network in ${COMPATIBLE//,/ }; do
  grep -q "^| $network | " <<<"$NEW_BODY" || { echo "Network $network not in the Networks Compatibility table" >&2; exit 2; }
  NEW_BODY="$(sed "s/^| $network | ⛔ |$/| $network | ✔ |/" <<<"$NEW_BODY")"
done
NEW_BODY="$(replace_section "## Cardano Node Compatibility" "$CARDANO" <<<"$NEW_BODY")"
[[ -z "$DMQ" ]] || NEW_BODY="$(replace_section "## DMQ Node Compatibility" "$DMQ" <<<"$NEW_BODY")"
printf '%s\n' "$NEW_BODY" >"$OUT"

CHANGES="$(diff <(printf '%s\n' "$BODY") "$OUT" | grep '^>' | sed 's/^> //')"
[[ -n "$CHANGES" ]] || { echo "No change: networks already compatible and tables up to date"; exit 1; }
echo "Changed lines:"
printf '%s\n' "$CHANGES"
echo
echo "gh release edit $TAG --repo $REPO --notes-file $OUT"
