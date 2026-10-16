#!/usr/bin/env bash
# Renders the notes of a pre-release: highlights file, GitHub generated notes, then the current body unchanged.
# Read-only: the generate-notes call creates nothing, the notes are written to a local file and the edit command is printed.
# Rights: Contents write (generate-notes and the printed release edit), public reads otherwise.
# Requirements: gh, grep.
# Exit codes: 0 notes rendered, 1 guard failed (not a pre-release or Latest changed), 2 usage or GitHub error.
# Usage:
#   bash release-notes.sh --tag <tag> --previous-tag <latest release tag> --commit <sha> --highlights <file> --out <file>
#   bash release-notes.sh --tag <tag> --previous-tag <latest release tag> --guard-only
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands gh grep

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
TAG="" PREVIOUS_TAG="" COMMIT="" HIGHLIGHTS="" OUT="" GUARD_ONLY=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --tag) TAG="${2:?}"; shift 2 ;;
    --previous-tag) PREVIOUS_TAG="${2:?}"; shift 2 ;;
    --commit) COMMIT="${2:?}"; shift 2 ;;
    --highlights) HIGHLIGHTS="${2:?}"; shift 2 ;;
    --out) OUT="${2:?}"; shift 2 ;;
    --guard-only) GUARD_ONLY=yes; shift ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ -n "$TAG" && -n "$PREVIOUS_TAG" ]] || usage_error
[[ -n "$GUARD_ONLY" || ( -n "$COMMIT" && -n "$HIGHLIGHTS" && -n "$OUT" ) ]] || usage_error
[[ -n "$GUARD_ONLY" || -s "$HIGHLIGHTS" ]] || { echo "Highlights file $HIGHLIGHTS is missing or empty" >&2; exit 2; }

guard() {
  local prerelease latest view
  if view="$(gh release view "$TAG" --repo "$REPO" --json isPrerelease --jq .isPrerelease 2>&1)"; then
    prerelease="$view"
  elif [[ "$view" == *"not found"* ]]; then
    prerelease=""
  else
    echo "Cannot read the release $TAG: $view" >&2
    exit 2
  fi
  latest="$(gh release view --repo "$REPO" --json tagName --jq .tagName)" || { echo "Cannot read the Latest release" >&2; exit 2; }
  echo "Guard: $TAG pre-release=${prerelease:-absent}, Latest=$latest"
  [[ "$latest" == "$PREVIOUS_TAG" ]] || { echo "Latest is $latest, expected $PREVIOUS_TAG" >&2; exit 1; }
  [[ -z "$prerelease" || "$prerelease" == "true" ]] || { echo "$TAG is not a pre-release" >&2; exit 1; }
  RELEASE_EXISTS="$([[ -n "$prerelease" ]] && echo yes || echo no)"
}

guard
[[ -z "$GUARD_ONLY" ]] || exit 0
CURRENT_BODY=""
[[ "$RELEASE_EXISTS" == yes ]] && CURRENT_BODY="$(gh release view "$TAG" --repo "$REPO" --json body --jq .body)"

{
  if grep -q '^## Highlights' <<<"$CURRENT_BODY"; then
    echo "Highlights already in the current body, not added" >&2
  else
    grep -q '^## Highlights' "$HIGHLIGHTS" || echo "## Highlights:"
    cat "$HIGHLIGHTS"
    echo
  fi
  if grep -q "^## What's Changed" <<<"$CURRENT_BODY"; then
    echo "What's Changed already in the current body, not added" >&2
  else
    gh api --method POST "repos/$REPO/releases/generate-notes" -f tag_name="$TAG" -f previous_tag_name="$PREVIOUS_TAG" \
      -f target_commitish="$COMMIT" --jq .body || { echo "generate-notes failed" >&2; exit 2; }
    echo
  fi
  printf '%s\n' "$CURRENT_BODY"
} >"$OUT" || exit 2

echo "Notes: $(grep -c '^\* .* in https://github.com/' "$OUT") PRs, $(grep -c 'made their first contribution' "$OUT") new contributors, $(wc -l <"$OUT") lines in $OUT"
[[ "$RELEASE_EXISTS" == yes ]] || echo "Release $TAG does not exist yet: render again after the pre-release workflow created it"
echo
echo "gh release edit $TAG --repo $REPO --notes-file $OUT --prerelease"
echo "bash $(dirname "$0")/release-notes.sh --tag $TAG --previous-tag $PREVIOUS_TAG --guard-only"
