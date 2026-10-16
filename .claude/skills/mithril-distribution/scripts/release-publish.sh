#!/usr/bin/env bash
# Renders the publication of a pre-release as the Latest release (it fires release.yml, which deploys the release networks), or verifies it.
# Read-only: prints the command, never edits the release.
# Rights: public reads; the printed release edit needs repository Write role (Contents write).
# Exit codes: 0 printed or verified, 1 a guard or the verification failed, 2 usage or GitHub error.
# Usage:
#   bash release-publish.sh print  --tag <tag> --previous-tag <latest release tag> [--baseline <file>]...
#   bash release-publish.sh verify --tag <tag>
set -uo pipefail

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
MODE="${1:-}"
shift || true
TAG="" PREVIOUS_TAG=""
BASELINES=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --tag) TAG="${2:?}"; shift 2 ;;
    --previous-tag) PREVIOUS_TAG="${2:?}"; shift 2 ;;
    --baseline) BASELINES+=("${2:?}"); shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ "$MODE" =~ ^(print|verify)$ && -n "$TAG" && ( "$MODE" == verify || -n "$PREVIOUS_TAG" ) ]] || { sed -n '6,8p' "$0" >&2; exit 2; }

RELEASE="$(gh release view "$TAG" --repo "$REPO" --json isPrerelease,isDraft --jq '"\(.isPrerelease) \(.isDraft)"')" || { echo "No release $TAG" >&2; exit 2; }
read -r PRERELEASE DRAFT <<<"$RELEASE"
LATEST="$(gh release view --repo "$REPO" --json tagName --jq .tagName)" || { echo "Cannot read the Latest release" >&2; exit 2; }
echo "$TAG: pre-release=$PRERELEASE, draft=$DRAFT, Latest=$LATEST"

FAILURES=0
check() { if eval "$2"; then echo "🟢 $1"; else echo "🔴 $1"; FAILURES=$((FAILURES + 1)); fi; }

if [[ "$MODE" == verify ]]; then
  check "$TAG is published (not a pre-release nor a draft)" '[[ "$PRERELEASE" == false && "$DRAFT" == false ]]'
  check "$TAG is the Latest release" '[[ "$LATEST" == "$TAG" ]]'
  [[ $FAILURES -eq 0 ]]
  exit $?
fi

check "$TAG is a pre-release, not a draft" '[[ "$PRERELEASE" == true && "$DRAFT" == false ]]'
check "Latest is still $PREVIOUS_TAG" '[[ "$LATEST" == "$PREVIOUS_TAG" ]]'
for baseline in "${BASELINES[@]}"; do
  check "baseline $(basename "$baseline") saved today" '[[ -f "$baseline" && "$(date -u -r "$baseline" +%F)" == "$(date -u +%F)" ]]'
done
[[ $FAILURES -eq 0 ]] || exit 1
echo
echo "gh release edit $TAG --repo $REPO --prerelease=false --latest"
