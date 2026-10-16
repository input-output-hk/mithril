#!/usr/bin/env bash
# Prints the gcloud commands the user runs to rotate the key of the service account used by the aggregator ancillary signer (GCP KMS).
# Read-only: never calls gcloud nor GitHub, never reads, prints nor stores key material.
# Rights: none to print; the printed commands need the Service Account Key Admin role (roles/iam.serviceAccountKeyAdmin)
#         on the service account, and updating the GitHub secret needs the Admin role on the repository (more than Write).
# Requirements: git (gcloud only to run the printed commands).
# Exit codes: 0 printed, 2 usage error.
# Usage:
#   bash kms-rotation-gcloud.sh --service-account <email> --key-file <path outside any repository>
#                               [--project <project id>] [--previous-key-id <key id>]
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands git

SERVICE_ACCOUNT="" KEY_FILE="" PROJECT="" PREVIOUS_KEY_ID=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --service-account) SERVICE_ACCOUNT="${2:?}"; shift 2 ;;
    --key-file) KEY_FILE="${2:?}"; shift 2 ;;
    --project) PROJECT="${2:?}"; shift 2 ;;
    --previous-key-id) PREVIOUS_KEY_ID="${2:?}"; shift 2 ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ "$SERVICE_ACCOUNT" == *@*.iam.gserviceaccount.com && -n "$KEY_FILE" ]] || usage_error
git -C "$(dirname "$KEY_FILE")" rev-parse --show-toplevel >/dev/null 2>&1 \
  && { echo "Key file $KEY_FILE is inside a git repository, choose a path outside any repository" >&2; exit 2; }

PROJECT_OPTION="${PROJECT:+ --project=$PROJECT}"
cat <<EOF
1. List the current user-managed keys, and note the id of the key in use (the previous key):
gcloud iam service-accounts keys list --iam-account=$SERVICE_ACCOUNT --managed-by=user$PROJECT_OPTION

2. Create the new key:
gcloud iam service-accounts keys create $KEY_FILE --iam-account=$SERVICE_ACCOUNT$PROJECT_OPTION

3. Store the base64 encoded content of $KEY_FILE in the AGGREGATOR_ANCILLARY_SIGNER_GCP_KMS_CREDENTIALS secret of the
   release-mainnet environment (repository Settings > Environments, Admin role), then remove the local key file:
shred -u $KEY_FILE

4. Once the release is deployed and a Cardano database snapshot with ancillary files is created, destroy the previous key:
gcloud iam service-accounts keys delete ${PREVIOUS_KEY_ID:-<previous key id>} --iam-account=$SERVICE_ACCOUNT$PROJECT_OPTION

5. Check that only the new key is left:
gcloud iam service-accounts keys list --iam-account=$SERVICE_ACCOUNT --managed-by=user$PROJECT_OPTION
EOF
