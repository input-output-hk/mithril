#!/usr/bin/env bash
# Read-only probe of one rollout step.
# Rights: public reads, except env-var and env-secret probes (Admin role, token Environments read)
#         and workflow-run and cache-empty probes (Actions read).
# Requirements: gh, curl, jq, python3 (checkbox probe).
# Exit codes: 0 done, 1 not done yet, 2 probe error, 3 failed (e.g. workflow run concluded with failure).
# Usage: bash detect-step.sh <probe> <args...>   (bash detect-step.sh help for the list)
set -uo pipefail

# shellcheck source=common.sh
source "$(dirname "${BASH_SOURCE[0]}")/common.sh" || exit 2
require_commands gh curl jq

REPO="${MITHRIL_REPO:-IntersectMBO/mithril}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GHCR_ACCEPT="application/vnd.oci.image.index.v1+json,application/vnd.docker.distribution.manifest.list.v2+json,application/vnd.docker.distribution.manifest.v2+json"

done_() { echo "DONE: $*"; exit 0; }
pending() { echo "PENDING: $*"; exit 1; }
error() { echo "ERROR: $*" >&2; exit 2; }
failed() { echo "FAILED: $*"; exit 3; }

usage() {
  script_help
  echo
  cat <<'EOF'
Probes:
  tag <tag>                                       Tag exists on GitHub
  workflow-run <workflow file> <since ISO> [<sha>]  A run created after <since> (on <sha>) succeeded
  env-var <environment> <name> <value>            Variable has the value
  env-var-absent <environment> <name>             Variable is absent
  env-secret <environment> <name>                 Secret exists
  env-secret-absent <environment> <name>          Secret is absent
  release-exists <tag>                            Release (or pre-release) exists for the tag
  release-published <tag>                         Release is published, not a draft nor a pre-release
  release-body-contains <tag> <text>              Release notes contain the text
  pr-merged <pr number>                           Pull request is merged
  checkbox <issue> <phase number> <step text>     How checkbox with this exact text is checked
                                                  (a child: '<parent text> > <child text>')
  npm-dist-tag <package> <dist tag> <version>     npm dist tag points to the version
  crate-version <crate> <version>                 Version is published on crates.io
  docker-images <image id>                        mithril-aggregator, mithril-signer, mithril-client images exist on ghcr.io
  cache-empty                                     No GitHub Actions cache left
  url-ok <url>                                    URL answers with HTTP 200
  aggregator-commit <network> <commit sha>        Aggregator of the network runs the commit
EOF
}

[[ $# -ge 1 ]] || { usage; exit 2; }
PROBE="$1"
shift

case "$PROBE" in
  help | --help | -h)
    usage
    exit 0
    ;;
  tag)
    TAG="${1:?tag}"
    SHA="$(gh api "repos/$REPO/commits/refs/tags/$TAG" --jq .sha 2>/dev/null)" && done_ "tag $TAG on $SHA"
    pending "tag $TAG not on GitHub"
    ;;
  workflow-run)
    WORKFLOW="${1:?workflow}" SINCE="${2:?since}" SHA="${3:-}"
    RUN="$(gh run list --repo "$REPO" --workflow "$WORKFLOW" --limit 30 \
      --json databaseId,status,conclusion,createdAt,headSha,url \
      --jq "[.[] | select(.createdAt >= \"$SINCE\") | select(\"$SHA\" == \"\" or (.headSha | startswith(\"$SHA\")))] | first // empty | \"\(.status) \(.conclusion) \(.url)\"")" \
      || error "cannot list runs of $WORKFLOW"
    [[ -n "$RUN" ]] || pending "no run of $WORKFLOW since $SINCE"
    read -r STATUS CONCLUSION URL <<<"$RUN"
    [[ "$STATUS" == "completed" ]] || pending "$WORKFLOW $STATUS $URL"
    [[ "$CONCLUSION" == "success" ]] && done_ "$WORKFLOW success $URL"
    failed "$WORKFLOW $CONCLUSION $URL"
    ;;
  env-var | env-var-absent)
    ENVIRONMENT="${1:?environment}" NAME="${2:?name}"
    VALUE="$(gh variable get "$NAME" --env "$ENVIRONMENT" --repo "$REPO" 2>&1)"
    STATUS=$?
    if [[ "$PROBE" == "env-var-absent" ]]; then
      [[ $STATUS -ne 0 && "$VALUE" == *"not found"* ]] && done_ "$NAME absent from $ENVIRONMENT"
      [[ $STATUS -eq 0 ]] && pending "$NAME still set in $ENVIRONMENT"
      error "$VALUE"
    fi
    EXPECTED="${3?value}"
    [[ $STATUS -eq 0 ]] || { [[ "$VALUE" == *"not found"* ]] && pending "$NAME not set in $ENVIRONMENT"; error "$VALUE"; }
    [[ "$VALUE" == "$EXPECTED" ]] && done_ "$NAME=$VALUE in $ENVIRONMENT"
    pending "$NAME=$VALUE in $ENVIRONMENT, expected $EXPECTED"
    ;;
  env-secret | env-secret-absent)
    ENVIRONMENT="${1:?environment}" NAME="${2:?name}"
    SECRETS="$(gh secret list --env "$ENVIRONMENT" --repo "$REPO" --json name --jq '.[].name')" || error "cannot list secrets of $ENVIRONMENT"
    if grep -qx "$NAME" <<<"$SECRETS"; then
      [[ "$PROBE" == "env-secret" ]] && done_ "secret $NAME in $ENVIRONMENT"
      pending "secret $NAME still in $ENVIRONMENT"
    fi
    [[ "$PROBE" == "env-secret-absent" ]] && done_ "secret $NAME absent from $ENVIRONMENT"
    pending "secret $NAME not in $ENVIRONMENT"
    ;;
  release-exists | release-published | release-body-contains)
    TAG="${1:?tag}"
    RELEASE="$(gh release view "$TAG" --repo "$REPO" --json isDraft,isPrerelease,url,body 2>&1)" \
      || { [[ "$RELEASE" == *"not found"* ]] && pending "no release for $TAG"; error "$RELEASE"; }
    case "$PROBE" in
      release-exists) done_ "release $TAG $(jq -r .url <<<"$RELEASE")" ;;
      release-published)
        [[ "$(jq -r '.isDraft or .isPrerelease' <<<"$RELEASE")" == "false" ]] && done_ "release $TAG published"
        pending "release $TAG is $(jq -r 'if .isDraft then "a draft" else "a pre-release" end' <<<"$RELEASE")"
        ;;
      release-body-contains)
        TEXT="${2:?text}"
        jq -r .body <<<"$RELEASE" | grep -qF -- "$TEXT" && done_ "release $TAG notes contain '$TEXT'"
        pending "release $TAG notes do not contain '$TEXT'"
        ;;
    esac
    ;;
  pr-merged)
    PR="${1:?pr}"
    STATE="$(gh pr view "$PR" --repo "$REPO" --json state,url --jq '"\(.state) \(.url)"')" || error "cannot read PR $PR"
    [[ "$STATE" == MERGED* ]] && done_ "PR $STATE"
    pending "PR $STATE"
    ;;
  checkbox)
    ISSUE="${1:?issue}" PHASE="${2:?phase}" TEXT="${3:?step text}"
    require_commands python3
    STEPS="$(python3 -I "$SCRIPT_DIR/issue-steps.py" "$ISSUE" "$PHASE" --json)" || error "cannot read issue $ISSUE"
    MATCHES="$(jq -c --arg text "$TEXT" '
      . as $steps
      | ($text | split(" > ")) as $parts
      | [.[] | select(
          if ($parts | length) == 2
          then .parent as $parent | .text == $parts[1] and $parent != null and ([$steps[] | select(.index == $parent) | .text] == [$parts[0]])
          else .text == $text
          end)]' <<<"$STEPS")"
    case "$(jq length <<<"$MATCHES")" in
      0) error "no step \"$TEXT\" in phase $PHASE of issue $ISSUE" ;;
      1) ;;
      *) error "several steps \"$TEXT\" in phase $PHASE of issue $ISSUE" ;;
    esac
    INDEX="$(jq -r '.[0].index' <<<"$MATCHES")"
    [[ "$(jq -r '.[0].checked' <<<"$MATCHES")" == "true" ]] && done_ "step $INDEX of phase $PHASE checked: $TEXT"
    pending "step $INDEX of phase $PHASE unchecked: $TEXT"
    ;;
  npm-dist-tag)
    PACKAGE="${1:?package}" DIST_TAG="${2:?dist tag}" VERSION="${3:?version}"
    CURRENT="$(curl -sf "https://registry.npmjs.org/-/package/$PACKAGE/dist-tags" | jq -r --arg tag "$DIST_TAG" '.[$tag] // empty')" \
      || error "cannot read npm dist tags of $PACKAGE"
    [[ "$CURRENT" == "$VERSION" ]] && done_ "$PACKAGE@$DIST_TAG is $VERSION"
    pending "$PACKAGE@$DIST_TAG is '${CURRENT}', expected $VERSION"
    ;;
  crate-version)
    CRATE="${1:?crate}" VERSION="${2:?version}"
    CODE="$(curl -s -o /dev/null -w '%{http_code}' -A "mithril-distribution-skill" "https://crates.io/api/v1/crates/$CRATE/$VERSION")"
    [[ "$CODE" == "200" ]] && done_ "$CRATE $VERSION on crates.io"
    [[ "$CODE" == "404" ]] && pending "$CRATE $VERSION not on crates.io"
    error "crates.io answered $CODE"
    ;;
  docker-images)
    IMAGE_ID="${1:?image id}"
    MISSING=()
    for PACKAGE in mithril-aggregator mithril-signer mithril-client; do
      TOKEN="$(curl -sf "https://ghcr.io/token?scope=repository:intersectmbo/$PACKAGE:pull" | jq -r .token)" || error "cannot get a ghcr.io token"
      CODE="$(curl -s -o /dev/null -w '%{http_code}' -H "Authorization: Bearer $TOKEN" -H "Accept: $GHCR_ACCEPT" \
        "https://ghcr.io/v2/intersectmbo/$PACKAGE/manifests/$IMAGE_ID")"
      [[ "$CODE" == "200" ]] || MISSING+=("$PACKAGE ($CODE)")
    done
    [[ ${#MISSING[@]} -eq 0 ]] && done_ "images $IMAGE_ID published"
    pending "images $IMAGE_ID missing: ${MISSING[*]}"
    ;;
  cache-empty)
    COUNT="$(gh cache list --repo "$REPO" --limit 1000 --json id --jq length)" || error "cannot list caches"
    [[ "$COUNT" == "0" ]] && done_ "no cache left"
    pending "$COUNT caches left"
    ;;
  url-ok)
    URL="${1:?url}"
    CODE="$(curl -s -o /dev/null -w '%{http_code}' -L "$URL")"
    [[ "$CODE" == "200" ]] && done_ "$URL answers 200"
    pending "$URL answers $CODE"
    ;;
  aggregator-commit)
    NETWORK="${1:?network}" SHA="${2:?sha}"
    VERSION="$(curl -sf "https://aggregator.$NETWORK.api.mithril.network/aggregator/status" | jq -r .aggregator_node_version)" \
      || pending "aggregator of $NETWORK not answering"
    [[ "${VERSION#*+}" == "${SHA:0:7}" ]] && done_ "$NETWORK runs $VERSION"
    pending "$NETWORK runs $VERSION, expected +${SHA:0:7}"
    ;;
  *)
    usage
    exit 2
    ;;
esac
