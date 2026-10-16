#!/usr/bin/env bash
# Helpers shared by the distribution skill scripts, sourced by each of them (not run on its own).
# script_help prints the header comment of the calling script, require_commands and require_gnu_date exit 2 when a
# requirement is missing.

script_help() { awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0"; }

handle_help() {
  local argument
  for argument in "$@"; do
    if [[ "$argument" == -h || "$argument" == --help ]]; then
      script_help
      exit 0
    fi
  done
}

require_commands() {
  local command missing=()
  for command in "$@"; do
    command -v "$command" >/dev/null 2>&1 || missing+=("$command")
  done
  [[ ${#missing[@]} -eq 0 ]] || { echo "Missing required commands: ${missing[*]}" >&2; exit 2; }
}

require_gnu_date() { date -u -d @0 +%s >/dev/null 2>&1 || { echo "GNU date is required (date -d)" >&2; exit 2; }; }
