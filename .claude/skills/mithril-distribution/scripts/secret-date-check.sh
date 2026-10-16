#!/usr/bin/env bash
# Checks that a GitHub environment secret was updated after a given time, from its update date only.
# Read-only: lists the secret names and update dates of the environment, never reads nor changes a secret value.
# Exit codes: 0 updated after the time, 1 not updated (or absent), 2 usage or GitHub error.
# Usage: bash secret-date-check.sh --environment <environment> --secret <name> --since <ISO>
set -uo pipefail

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
[[ -n "$ENVIRONMENT" && -n "$SECRET" && -n "$SINCE" ]] || { sed -n '5p' "$0" >&2; exit 2; }

UPDATED_AT="$(gh secret list --env "$ENVIRONMENT" --repo "$REPO" --json name,updatedAt --jq ".[] | select(.name == \"$SECRET\") | .updatedAt")" \
  || { echo "Cannot list the secrets of $ENVIRONMENT" >&2; exit 2; }
echo "$SECRET of $ENVIRONMENT last updated: ${UPDATED_AT:-absent}"
if [[ -n "$UPDATED_AT" && "$(date -u -d "$UPDATED_AT" +%s)" -gt "$(date -u -d "$SINCE" +%s)" ]]; then
  echo "🟢 updated after $SINCE"
  exit 0
fi
echo "🔴 not updated after $SINCE"
exit 1
