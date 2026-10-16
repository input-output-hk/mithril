#!/usr/bin/env bash
# Renders one batch of GitHub environment variable changes, or verifies that the batch is applied.
# Read-only: reads the variables of the environment, prints the commands.
# Rights: Admin role on the repository (environment variables are repository settings), token Environments read to list
#         and Environments write for the printed set and delete commands. A warning is printed below the Admin role.
# A name with '*' is expanded against the variables of the environment.
# Exit codes: 0 printed or verified, 1 verification failed or nothing to change, 2 usage or GitHub error.
# Usage:
#   bash env-commands.sh print  --environment <environment> [set <NAME>=<value>]... [delete <NAME or pattern>]...
#   bash env-commands.sh verify --environment <environment> [set <NAME>=<value>]... [delete <NAME or pattern>]...
set -uo pipefail

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
MODE="${1:-}"
shift || true
ENVIRONMENT=""
OPERATIONS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --environment) ENVIRONMENT="${2:?}"; shift 2 ;;
    set | delete) OPERATIONS+=("$1 ${2:?}"); shift 2 ;;
    *) echo "Unknown argument $1" >&2; exit 2 ;;
  esac
done
[[ "$MODE" =~ ^(print|verify)$ && -n "$ENVIRONMENT" && ${#OPERATIONS[@]} -gt 0 ]] || { sed -n '8,10p' "$0" >&2; exit 2; }

role_warning() {
  local role
  role="$(gh api "repos/$REPO" --jq '.permissions | if .admin then "admin" elif .maintain then "maintain" elif .push then "write" elif .triage then "triage" else "read" end' 2>/dev/null)"
  [[ "$role" == admin ]] || echo "⚠️  $1 needs the Admin role on $REPO (more than Write), your role: ${role:-unknown}" >&2
}
role_warning "Managing environment variables"
VARIABLES="$(gh variable list --env "$ENVIRONMENT" --repo "$REPO" --json name,value)" || { echo "Cannot read $ENVIRONMENT" >&2; exit 2; }
current() { jq -r --arg n "$1" '.[] | select(.name == $n) | .value' <<<"$VARIABLES"; }

COMMANDS=() CHECKS=()
for operation in "${OPERATIONS[@]}"; do
  read -r verb argument <<<"$operation"
  if [[ "$verb" == set ]]; then
    NAME="${argument%%=*}" VALUE="${argument#*=}"
    COMMANDS+=("gh variable set $NAME --env $ENVIRONMENT --repo $REPO --body '$VALUE'")
    CHECKS+=("$NAME|set|$VALUE")
  else
    NAMES="$(jq -r --arg p "$argument" '.[] | .name | select(test("^" + ($p | gsub("\\*"; ".*")) + "$"))' <<<"$VARIABLES")"
    [[ -n "$NAMES" || "$MODE" == verify || "$argument" != *'*'* ]] || { echo "No variable matches $argument in $ENVIRONMENT" >&2; exit 1; }
    [[ -n "$NAMES" ]] || NAMES="$argument"
    for NAME in $NAMES; do
      COMMANDS+=("gh variable delete $NAME --env $ENVIRONMENT --repo $REPO")
      CHECKS+=("$NAME|delete|")
    done
  fi
done

FAILURES=0
echo "| Variable | Current | Expected | |"
echo "|---|---|---|---|"
for check in "${CHECKS[@]}"; do
  IFS='|' read -r NAME VERB VALUE <<<"$check"
  NOW="$(current "$NAME")"
  if [[ "$VERB" == set ]]; then
    [[ "$NOW" == "$VALUE" ]] && STATE="🟢" || STATE="🔴"
    echo "| \`$NAME\` | \`${NOW:-absent}\` | \`$VALUE\` | $STATE |"
  else
    [[ -z "$NOW" ]] && STATE="🟢" || STATE="🔴"
    echo "| \`$NAME\` | \`${NOW:-absent}\` | absent | $STATE |"
  fi
  [[ "$STATE" == "🟢" ]] || FAILURES=$((FAILURES + 1))
done

if [[ "$MODE" == verify ]]; then
  [[ $FAILURES -eq 0 ]]
  exit $?
fi
echo
printf '%s\n' "${COMMANDS[@]}"
