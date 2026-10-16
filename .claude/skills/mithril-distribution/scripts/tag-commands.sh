#!/usr/bin/env bash
# Prints the two blocks the user runs to tag a distribution on main and push the tag, and checks the result of the first one.
# Read-only: never creates, signs nor pushes a tag.
# Exit codes: 0 printed or check passed, 1 check failed, 2 usage error.
# Usage:
#   bash tag-commands.sh print --tag <distribution>.<patch> --commit <sha>
#   bash tag-commands.sh check --tag <distribution>.<patch> --commit <sha> [--repository <path>]
set -uo pipefail

MODE="${1:-}"
shift || true
TAG="" COMMIT="" REPOSITORY="."
while [[ $# -gt 0 ]]; do
  case "$1" in
    --tag) TAG="${2:?}"; shift 2 ;;
    --commit) COMMIT="${2:?}"; shift 2 ;;
    --repository) REPOSITORY="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ "$MODE" =~ ^(print|check)$ && "$TAG" =~ ^[0-9]{4}\.[0-9]+ && -n "$COMMIT" ]] || { sed -n '5,7p' "$0" >&2; exit 2; }

if [[ "$MODE" == print ]]; then
  cat <<EOF
Step 1, create the tag on main:

DISTRIBUTION_VERSION=${TAG%%.*}
PATCH_VERSION=${TAG#*.}
COMMIT_ID=$COMMIT

git switch main
git pull --rebase
git tag -s \$DISTRIBUTION_VERSION.\$PATCH_VERSION \$COMMIT_ID -m "\$DISTRIBUTION_VERSION.\$PATCH_VERSION release"
git log --oneline --decorate -1

Step 2, push the tag (after the check of step 1):

git push origin $TAG
EOF
  exit 0
fi

LINE="$(git -C "$REPOSITORY" log --oneline --decorate -1 main)" || exit 2
echo "$LINE"
FAILURES=0
check() { if eval "$2"; then echo "🟢 $1"; else echo "🔴 $1"; FAILURES=$((FAILURES + 1)); fi; }
check "current branch is main" '[[ "$(git -C "$REPOSITORY" branch --show-current)" == main ]]'
check "main, origin/main and the tag point to ${COMMIT:0:9}" \
  '[[ "$(git -C "$REPOSITORY" rev-parse main origin/main "$TAG^{commit}" 2>/dev/null | sort -u)" == "$(git -C "$REPOSITORY" rev-parse "$COMMIT")" ]]'
check "git log line carries HEAD -> main, origin/main and tag: $TAG" \
  '[[ "$LINE" == *"HEAD -> main"* && "$LINE" == *"origin/main"* && "$LINE" == *"tag: $TAG"* ]]'
check "tag signature is good" 'git -C "$REPOSITORY" tag -v "$TAG" >/dev/null 2>&1'
[[ $FAILURES -eq 0 ]]
