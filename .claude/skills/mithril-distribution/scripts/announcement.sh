#!/usr/bin/env bash
# Prints one announcement message of a drafts file as raw Markdown, and checks it is ready to post.
# Messages are separated by a line of dashes ('-------').
# Rights: none (local file).
# Requirements: awk, sed, cut, sort, grep with -P (GNU grep).
# Exit codes: 0 printed and ready, 1 printed with leftover placeholders or Unicode emojis, 2 usage error.
# Usage:
#   bash announcement.sh --file <drafts file> --list
#   bash announcement.sh --file <drafts file> --message <number>
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
handle_help "$@"
require_commands awk sed cut sort grep
grep -qP '' <<<'' 2>/dev/null || { echo "grep with -P (GNU grep) is required" >&2; exit 2; }

FILE="" MESSAGE="" LIST=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --file) FILE="${2:?}"; shift 2 ;;
    --message) MESSAGE="${2:?}"; shift 2 ;;
    --list) LIST=yes; shift ;;
    *) echo "Unknown option $1" >&2; exit 2 ;;
  esac
done
[[ -f "$FILE" && ( -n "$LIST" || "$MESSAGE" =~ ^[0-9]+$ ) ]] || usage_error

split() { awk -v wanted="$1" 'BEGIN { n = 1 } /^-------+$/ { n++; next } n == wanted || wanted == 0 { print n "\t" $0 }' "$FILE"; }

if [[ -n "$LIST" ]]; then
  split 0 | awk -F'\t' 'NF > 1 && $2 != "" && !seen[$1]++ { print $1 ": " substr($2, 1, 100) }'
  exit 0
fi

TEXT="$(split "$MESSAGE" | cut -f2- | sed -e '/./,$!d' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}')"
[[ -n "$TEXT" ]] || { echo "No message $MESSAGE in $FILE" >&2; exit 2; }
printf '%s\n' "$TEXT"

ISSUES=0
PLACEHOLDERS="$(grep -n -o -E '26XX|XXXX|TBD|<[A-Z_]+>' <<<"$TEXT" | sort -u)"
[[ -z "$PLACEHOLDERS" ]] || { echo "Leftover placeholders: $(tr '\n' ' ' <<<"$PLACEHOLDERS")" >&2; ISSUES=1; }
EMOJIS="$(grep -n -o -P '[\x{1F000}-\x{1FAFF}\x{2600}-\x{27BF}]' <<<"$TEXT" | sort -u)"
[[ -z "$EMOJIS" ]] || { echo "Unicode emojis (use shortcodes): $(tr '\n' ' ' <<<"$EMOJIS")" >&2; ISSUES=1; }
exit $ISSUES
