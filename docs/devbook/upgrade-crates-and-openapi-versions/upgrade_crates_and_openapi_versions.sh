#!/usr/bin/env bash
set +a -u -o pipefail

if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi

# Check crates modify against `origin/main` and update their version.
# The openapi.yam is also verified and updated if necessary.
# At the end of the script, the commit message to used is displayed.

# Usage:
# update_crate_versions.sh --run: to execute the changes (default is dry-run)
# update_crate_versions.sh: to display the changes without executing them (dry-run mode)

# Prerequisites:
# `cargo-get` needs to be installed (`cargo install cargo-get`).

# NOTE
# `cargo get workspace.members` display the list of path to crates in the workspace.
# for the `cargo set-version` command, we need the name of the module (last element of the path).
readonly ORANGE="\e[1;33m"
readonly GREEN="\e[1;32m"
readonly RESET="\e[0m"

readonly OPEN_API_FILE=openapi.yaml
declare OPEN_API_UPDATE=""
declare OPEN_API_UPDATE_MESSAGE=""

update_crate_versions() {
    local -r dry_run=$1
    local -r -n files_modify=$2
    local -r -a members="$(cargo get workspace.members --delimiter " ")"
    local -i nb_files_modify=0
    local package_name

    local cargo_options=""
    if [ true = "$dry_run" ]
    then
        cargo_options=--dry-run
    fi

    for member in $members
    do
        nb_files_modify=$(echo "$files_modify" | grep -c "^$member/")
        if [[ $nb_files_modify -gt 0 ]]
        then
            package_name=${member##*/}
            cargo set-version $cargo_options --package "${package_name##*/}" --bump patch
        fi

    done
}

update_openapi_version() {
    local -r dry_run=$1
    local -r version_line=$(grep -E "version: [0-9]+\.[0-9]+\.[0-9]+" $OPEN_API_FILE)
    local -r patch_number=$(echo "$version_line" | cut -d . -f 3)
    local -r next_patch_number=$((patch_number + 1))
    local -r new_version=$(echo "$version_line" | cut -d . -f 1-2).$next_patch_number

    echo -e "   ${GREEN}Upgrading${RESET} $OPEN_API_FILE from ${version_line##*version: } to ${new_version##*version: }"
    if [ true = "$dry_run" ]
    then
        echo -e "${ORANGE}warning${RESET}: aborting $OPEN_API_FILE update due to dry run"
    else
        sed -i "s/$version_line/$new_version/g" $OPEN_API_FILE
    fi
    OPEN_API_UPDATE="\n* $OPEN_API_FILE from \`${version_line##*version: }\` to \`${new_version##*version: }\`"
    OPEN_API_UPDATE_MESSAGE=" and \`$OPEN_API_FILE\` version"
}

################
declare DRY_RUN=true
declare COMMIT=false
readonly COMMIT_REF="HEAD"
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --run) DRY_RUN=false ;;
        --commit) COMMIT=true ;;
    esac
    shift
done

FILES_MODIFY="$(git diff "$COMMIT_REF" --name-only origin/main)"
readonly -a FILES_MODIFY

update_crate_versions $DRY_RUN FILES_MODIFY

if [ "$(echo "${FILES_MODIFY[@]}" | grep -xc "$OPEN_API_FILE")" -gt 0 ]
then
    update_openapi_version $DRY_RUN
fi

if [ true = $DRY_RUN ]
then
    echo -e "${ORANGE}warning${RESET}: script is run in dry mode. To execute the changes, run ${GREEN}./update_crate_versions.sh --run${RESET}"
else
  UPDATED_CRATES="$(find . -name Cargo.toml -exec git diff "$COMMIT_REF" {} + | grep -E "^[\+\-]version = \"[0-9\.]+\"|name = " | tr '\n' ' ' | sed -r "s/ name = \"([a-z\-]+)\" -version = \"([0-9\.]+)\" \+version = \"([0-9\.]+)\" ?/* \1 from \`\2\` to \`\3\`\n/g")"
  if [[ -n $UPDATED_CRATES ]]
  then
    UPDATED_CRATES="\n${UPDATED_CRATES}"
  fi

  COMMIT_MESSAGE=$(echo -e "chore: upgrade crate versions${OPEN_API_UPDATE_MESSAGE}\n${UPDATED_CRATES}${OPEN_API_UPDATE}")

  echo -e "$COMMIT_MESSAGE"

  if [ true = $COMMIT ]
  then
    git add --update $OPEN_API_FILE Cargo.lock ./*/Cargo.toml ./internal/*/Cargo.toml ./mithril-test-lab/*/Cargo.toml
    git commit -m "$COMMIT_MESSAGE"
  fi
fi
