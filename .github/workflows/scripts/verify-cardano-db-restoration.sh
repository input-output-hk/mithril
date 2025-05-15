#!/bin/bash

set -e

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <file containing 'mithril-client' CLI download output> [--include-ancillary]"
  echo "The first argument must be the file path containing the raw output (stdout) of the mithril-client CLI download command."
  echo "The second optional argument must be '--include-ancillary' if ancillary files are included."
  exit 1
fi

if [[ ! -f "$1" ]]; then
  echo "Error: File '$1' not found."
  exit 1
fi

CLIENT_CMD_OUTPUT=$(cat "$1")
INCLUDE_ANCILLARY="$2"

DOCKER_CMD=$(echo "$CLIENT_CMD_OUTPUT" | grep -E '^\s*docker run')
if [[ -z "$DOCKER_CMD" ]]; then
  echo "No Docker command found in mithril-client CLI command output."
  exit 1
fi

echo "Extracted Docker command:"
echo "$DOCKER_CMD"

DOCKER_CMD_DETACHED="${DOCKER_CMD/docker run/docker run -d}"
echo "Running Docker command in detached mode:"
echo "$DOCKER_CMD_DETACHED"

CONTAINER_ID=$(eval "$DOCKER_CMD_DETACHED")

wait_for_log() {
  local CONTAINER_ID="$1"
  local TIMEOUT="$2"
  local LOG_MESSAGE="$3"

  echo "Waiting up to $TIMEOUT seconds for '$LOG_MESSAGE'..."
  for ((i=1; i<=TIMEOUT; i++)); do
    if docker logs "$CONTAINER_ID" 2>&1 | grep -q "$LOG_MESSAGE"; then
      echo "Found '$LOG_MESSAGE' in logs."
      return 0
    fi
    sleep 1
  done

  echo "'$LOG_MESSAGE' not found within $TIMEOUT seconds."
  docker logs "$CONTAINER_ID"
  return 1
}

if wait_for_log "$CONTAINER_ID" 15 "Started opening Immutable DB"; then
  echo "✅ The Cardano node started successfully from the Mithril snapshot."
else
  echo "❌ Failed to start the Cardano node from the Mithril snapshot."
  exit 1
fi

if [[ "$INCLUDE_ANCILLARY" == "--include-ancillary" ]]; then
  echo "Parameter '--include-ancillary' is set."
  if wait_for_log "$CONTAINER_ID" 30 "Chain extended, new tip"; then
    echo "✅ The Cardano node started successfully from the Mithril snapshot with the ancillary files."
    exit 0
  else
    echo "❌ Failed to start the Cardano node from the Mithril snapshot with the ancillary files."
    exit 1
  fi
else
  echo "Parameter '--include-ancillary' is not set."
  echo "Skipping additional check for 'Chain extended, new tip' in logs."
  exit 0
fi
