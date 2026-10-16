#!/usr/bin/env bash
# Resolves the rollout context of a Mithril distribution and prints it as KEY=value lines.
# Read-only: only GitHub API reads.
# Rights: an authenticated gh (public reads of IntersectMBO/mithril only).
# Locations: MITHRIL_DISTRIBUTION_LOG_DIR (rollout logs) and MITHRIL_DISTRIBUTION_ANNOUNCEMENTS_DIR (announcement drafts),
# both under ~/.mithril-distribution by default; REPOSITORY_DIR is the main checkout of the current git repository.
# Requirements: gh, git, jq, base64, awk.
# Usage: bash resolve-context.sh <distribution> [--commit <sha>] [--patch <n>]
set -euo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands gh git jq base64 awk

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"

[[ $# -ge 1 ]] || usage_error
DISTRIBUTION="$1"
shift
[[ "$DISTRIBUTION" =~ ^[0-9]{4}$ ]] || { echo "Distribution must be 4 digits, got '$DISTRIBUTION'" >&2; exit 2; }

COMMIT_OVERRIDE=""
PATCH_OVERRIDE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --commit) COMMIT_OVERRIDE="${2:?}"; shift 2 ;;
    --patch) PATCH_OVERRIDE="${2:?}"; shift 2 ;;
    *) usage_error ;;
  esac
done

emit() { printf '%s=%s\n' "$1" "$2"; }

find_release_issue() {
  gh issue list --repo "$REPO" --state all --limit 20 \
    --search "Release \`$1\` distribution in:title" \
    --json number,title \
    --jq ".[] | select(.title | test(\"Release .$1. distribution\")) | \"\(.number) \(.title)\""
}

read_version() {
  local commit="$1" path="$2" field="$3"
  gh api "repos/$REPO/contents/$path?ref=$commit" --jq .content 2>/dev/null \
    | base64 -d 2>/dev/null \
    | if [[ "$field" == "toml" ]]; then grep -m1 '^version' | cut -d'"' -f2; else jq -r .version; fi \
    || true
}

emit REPO "$REPO"
emit DISTRIBUTION "$DISTRIBUTION"

ISSUES="$(find_release_issue "$DISTRIBUTION")"
ISSUE_COUNT="$(grep -c . <<<"$ISSUES" || true)"
if [[ "$ISSUE_COUNT" == "1" ]]; then
  emit ISSUE_NUMBER "${ISSUES%% *}"
  emit ISSUE_TITLE "${ISSUES#* }"
else
  emit ISSUE_NUMBER ""
  emit ISSUE_CANDIDATES "$(tr '\n' '|' <<<"$ISSUES" | sed 's/|$//')"
fi

RELEASED_TAGS="$(gh release list --repo "$REPO" --limit 100 --exclude-pre-releases --json tagName --jq '.[].tagName' \
  | grep -E '^[0-9]{4}\.[0-9]+' \
  | awk -v d="$DISTRIBUTION" 'substr($0, 1, 4) < d' \
  | sort -V)"
PREVIOUS_TAG="$(tail -1 <<<"$RELEASED_TAGS")"
PREVIOUS_DISTRIBUTION="${PREVIOUS_TAG:0:4}"
emit PREVIOUS_TAG "$PREVIOUS_TAG"
emit PREVIOUS_DISTRIBUTION "$PREVIOUS_DISTRIBUTION"
emit PREVIOUS_ISSUE_NUMBER "$(find_release_issue "$PREVIOUS_DISTRIBUTION" | head -1 | cut -d' ' -f1)"
OLDER_DISTRIBUTION="$(cut -c1-4 <<<"$RELEASED_TAGS" | uniq | tail -2 | head -1)"
emit OLDER_DISTRIBUTION "$OLDER_DISTRIBUTION"
emit OLDER_ISSUE_NUMBER "$(find_release_issue "$OLDER_DISTRIBUTION" | head -1 | cut -d' ' -f1)"

EXISTING_TAGS="$(gh api "repos/$REPO/git/matching-refs/tags/$DISTRIBUTION." --jq '.[].ref' \
  | sed 's#refs/tags/##' | sort -V)"
emit EXISTING_TAGS "$(tr '\n' ' ' <<<"$EXISTING_TAGS" | sed 's/ $//')"
LATEST_TAG="$(tail -1 <<<"$EXISTING_TAGS")"
emit LATEST_TAG "$LATEST_TAG"
if [[ -n "$LATEST_TAG" ]]; then
  LATEST_TAG_COMMIT="$(gh api "repos/$REPO/commits/refs/tags/$LATEST_TAG" --jq .sha)"
  emit LATEST_TAG_COMMIT "$LATEST_TAG_COMMIT"
  emit LATEST_IMAGE_ID "$LATEST_TAG-${LATEST_TAG_COMMIT:0:7}"
else
  emit LATEST_TAG_COMMIT ""
  emit LATEST_IMAGE_ID ""
fi

if [[ -n "$PATCH_OVERRIDE" ]]; then
  PATCH="$PATCH_OVERRIDE"
elif [[ -n "$EXISTING_TAGS" ]]; then
  PATCH=$(( $(sed -E "s/^$DISTRIBUTION\.([0-9]+).*/\1/" <<<"$EXISTING_TAGS" | sort -n | tail -1) + 1 ))
else
  PATCH=0
fi
emit NEXT_PATCH "$PATCH"
emit NEXT_TAG "$DISTRIBUTION.$PATCH"

MAIN_COMMIT="$(gh api "repos/$REPO/commits/main" --jq .sha)"
emit MAIN_COMMIT "$MAIN_COMMIT"
if [[ -n "$COMMIT_OVERRIDE" ]]; then
  COMMIT="$(gh api "repos/$REPO/commits/$COMMIT_OVERRIDE" --jq .sha)"
else
  COMMIT="$MAIN_COMMIT"
fi
emit COMMIT "$COMMIT"
emit COMMIT_TITLE "$(gh api "repos/$REPO/commits/$COMMIT" --jq '.commit.message | split("\n")[0]')"
emit NEXT_IMAGE_ID "$DISTRIBUTION.$PATCH-${COMMIT:0:7}"

emit SIGNER_VERSION "$(read_version "$COMMIT" mithril-signer/Cargo.toml toml)"
emit AGGREGATOR_VERSION "$(read_version "$COMMIT" mithril-aggregator/Cargo.toml toml)"
emit CLIENT_CLI_VERSION "$(read_version "$COMMIT" mithril-client-cli/Cargo.toml toml)"
emit CLIENT_LIBRARY_VERSION "$(read_version "$COMMIT" mithril-client/Cargo.toml toml)"
emit CLIENT_WASM_VERSION "$(read_version "$COMMIT" mithril-client-wasm/package.json json)"

LOG_DIR="${MITHRIL_DISTRIBUTION_LOG_DIR:-$HOME/.mithril-distribution/logs}"
ANNOUNCEMENTS_DIR="${MITHRIL_DISTRIBUTION_ANNOUNCEMENTS_DIR:-$HOME/.mithril-distribution/announcements}"
emit REPOSITORY_DIR "$(git worktree list --porcelain 2>/dev/null | awk '/^worktree /{print $2; exit}')"
emit LOG_DIR "$LOG_DIR"
emit LOG_FILE "$LOG_DIR/$DISTRIBUTION-ROLLOUT.md"
emit ANNOUNCEMENTS_DIR "$ANNOUNCEMENTS_DIR"
emit ANNOUNCEMENT_FILE "$ANNOUNCEMENTS_DIR/mithril-$DISTRIBUTION.md"
emit ANNOUNCEMENT_STYLE_FILES "$(ls "$ANNOUNCEMENTS_DIR"/mithril-[0-9][0-9][0-9][0-9].md 2>/dev/null \
  | awk -F'mithril-|\\.md' -v d="$DISTRIBUTION" '$(NF-1) < d' | sort -V | tail -3 | tac | tr '\n' ' ' | sed 's/ $//')"
