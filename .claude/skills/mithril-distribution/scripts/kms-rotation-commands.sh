#!/usr/bin/env bash
# Prints the commands the user runs to rotate the service account key of the aggregator ancillary signer (GCP KMS),
# and verifies the GitHub side: the credentials secret of the environment was updated after a given time.
# Read-only: never calls GCP, never reads nor prints the key file.
# Exit codes: 0 printed or verified, 1 secret not updated since the given time, 2 usage or GitHub error.
# Usage:
#   bash kms-rotation-commands.sh print  --service-account <email> --environment <environment> --key-file <path>
#   bash kms-rotation-commands.sh verify --environment <environment> --since <ISO>
set -uo pipefail

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
SECRET=AGGREGATOR_ANCILLARY_SIGNER_GCP_KMS_CREDENTIALS
MODE="${1:-}"
shift || true
SERVICE_ACCOUNT="" ENVIRONMENT="" KEY_FILE="" SINCE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --service-account) SERVICE_ACCOUNT="${2:?}"; shift 2 ;;
    --environment) ENVIRONMENT="${2:?}"; shift 2 ;;
    --key-file) KEY_FILE="${2:?}"; shift 2 ;;
    --since) SINCE="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
usage() { sed -n '6,8p' "$0" >&2; exit 2; }

UPDATED_AT="$(gh secret list --env "${ENVIRONMENT:?}" --repo "$REPO" --json name,updatedAt --jq ".[] | select(.name == \"$SECRET\") | .updatedAt")" \
  || { echo "Cannot list the secrets of $ENVIRONMENT" >&2; exit 2; }

case "$MODE" in
  print)
    [[ -n "$SERVICE_ACCOUNT" && -n "$KEY_FILE" ]] || usage
    echo "$SECRET of $ENVIRONMENT last updated: ${UPDATED_AT:-absent}"
    cat <<EOF

1. List the current keys (note the id of the key to destroy):
gcloud iam service-accounts keys list --iam-account=$SERVICE_ACCOUNT --managed-by=user

2. Create the new key and store it in the environment (the key file never leaves this machine and is removed right after):
gcloud iam service-accounts keys create $KEY_FILE --iam-account=$SERVICE_ACCOUNT
base64 -w0 $KEY_FILE | gh secret set $SECRET --env $ENVIRONMENT --repo $REPO
shred -u $KEY_FILE

3. Destroy the previous key (once the new credentials are deployed and a snapshot with ancillary files is created):
gcloud iam service-accounts keys delete <previous key id> --iam-account=$SERVICE_ACCOUNT
EOF
    ;;
  verify)
    [[ -n "$SINCE" ]] || usage
    echo "$SECRET of $ENVIRONMENT last updated: ${UPDATED_AT:-absent}"
    [[ -n "$UPDATED_AT" && "$(date -u -d "$UPDATED_AT" +%s)" -gt "$(date -u -d "$SINCE" +%s)" ]] && echo "🟢 updated after $SINCE" && exit 0
    echo "🔴 not updated after $SINCE"
    exit 1
    ;;
  *) usage ;;
esac
