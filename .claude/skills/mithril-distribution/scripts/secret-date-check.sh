#!/usr/bin/env bash
# Checks that a GitHub environment secret was updated after a given time, from its update date only.
# Read-only: lists the secret names and update dates of the environment, never reads nor changes a secret value.
# Rights: Admin role on the repository (environment secrets are repository settings), token Environments read
#         (secret names and update dates only). A warning is printed below the Admin role.
# Requirements: gh, GNU date.
# Exit codes: 0 updated after the time, 1 not updated (or absent), 2 usage or GitHub error.
# Usage: bash secret-date-check.sh --environment <environment> --secret <name> --since <ISO>
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands gh
require_gnu_date

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
ENVIRONMENT="" SECRET="" SINCE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --environment) ENVIRONMENT="${2:?}"; shift 2 ;;
    --secret) SECRET="${2:?}"; shift 2 ;;
    --since) SINCE="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ -n "$ENVIRONMENT" && -n "$SECRET" && -n "$SINCE" ]] || usage_error

role_warning() {
  local role
  role="$(gh api "repos/$REPO" --jq '.permissions | if .admin then "admin" elif .maintain then "maintain" elif .push then "write" elif .triage then "triage" else "read" end' 2>/dev/null)"
  [[ "$role" == admin ]] || echo "⚠️  $1 needs the Admin role on $REPO (more than Write), your role: ${role:-unknown}" >&2
}
role_warning "Listing environment secrets"
UPDATED_AT="$(gh secret list --env "$ENVIRONMENT" --repo "$REPO" --json name,updatedAt --jq ".[] | select(.name == \"$SECRET\") | .updatedAt")" \
  || { echo "Cannot list the secrets of $ENVIRONMENT" >&2; exit 2; }
echo "$SECRET of $ENVIRONMENT last updated: ${UPDATED_AT:-absent}"
if [[ -n "$UPDATED_AT" && "$(date -u -d "$UPDATED_AT" +%s)" -gt "$(date -u -d "$SINCE" +%s)" ]]; then
  echo "🟢 updated after $SINCE"
  exit 0
fi
echo "🔴 not updated after $SINCE"
exit 1
