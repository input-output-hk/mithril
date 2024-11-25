#!/usr/bin/env bash
set +a -u -o pipefail

if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi

display_help() {
    echo "Check crates, js packages, and openapi changes against 'origin/main' and update their versions"
    echo
    echo "By default, no changes are made (dry-run mode), use '--run' to apply the changes."
    echo "At the end of the script, the commit message to used is displayed, use '--commit' to commit the changes."
    echo
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  --run: to apply the changes (default is dry-run)"
    echo "  --commit: to commit the changes"
    echo
    echo "Prerequisites:"
    echo " 'cargo-get' needs to be installed ('cargo install cargo-get')."
    echo
    exit 0;
}

check_requirements() {
    cargo get --version 2> /dev/null 1> /dev/null ||
        error "It seems 'cargo-get' is not installed or not in the path (to install run: 'cargo install cargo-get').";
}

error() {
    echo "ERROR: $1";
    exit 1;
}

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

update_package_json_versions() {
    local -r dry_run=$1
    local -r -n files_modify=$2
    local -r -n package_json_files=$3
    local -r -a members=$(echo "$package_json_files" | xargs dirname)
    local -i nb_files_modify=0

    local package_name version_line patch_number next_patch_number new_version must_update_package_locks
    must_update_package_locks=false

    for member in $members
    do
        nb_files_modify=$(echo "$files_modify" | grep -c "^$member/")
        if [[ $nb_files_modify -gt 0 ]]
        then
            if [[ $member == "mithril-client-wasm" ]]
            then
              must_update_package_locks=true
            fi

            version_line="$(grep -E "\"version\": \"[0-9]+\.[0-9]+\.[0-9]+\"" "$member/package.json" | sed "s/[\",]//g")"
            patch_number=$(echo "$version_line" | cut -d . -f 3)
            next_patch_number=$((patch_number + 1))
            new_version=$(echo "$version_line" | cut -d . -f 1-2).$next_patch_number
            echo -e "   ${GREEN}Upgrading${RESET} [js] $member from ${version_line##*version: } to ${new_version##*version: }"

            if [ true = "$dry_run" ]
            then
              echo -e "${ORANGE}warning${RESET}: aborting $member update due to dry run"
            else
              package_name=${member##*/}
              pushd "$member" > /dev/null || exit
              npm --no-git-tag-version version patch 1>/dev/null
              popd > /dev/null || exit
            fi
        fi
    done

    if [ true = "$must_update_package_locks" ]
    then
      echo -e "${ORANGE}mithril-client-wasm version changed${RESET}: updating package-lock.json files to reflect the new version"

      for member in $members
      do
          if [[ false = "$dry_run" && -e "$member/package-lock.json" ]]
          then
              package_name=${member##*/}
              pushd "$member" > /dev/null || exit
              npm install 1>/dev/null
              popd > /dev/null || exit
          fi
      done
    fi
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
check_requirements

declare DRY_RUN=true
declare COMMIT=false
readonly COMMIT_REF="HEAD"
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -h|--help) display_help ;;
        --run) DRY_RUN=false ;;
        --commit) COMMIT=true ;;
    esac
    shift
done

FILES_MODIFY="$(git diff "$COMMIT_REF" --name-only origin/main)"
readonly -a FILES_MODIFY
PACKAGE_JSON_FILES="$(find -- * -name package.json | grep -v -e "/node_modules/" -e "/pkg/" -e "/dist/" -e "/.next/" -e "docs/website/")"
readonly -a PACKAGE_JSON_FILES

update_crate_versions $DRY_RUN FILES_MODIFY
update_package_json_versions $DRY_RUN FILES_MODIFY PACKAGE_JSON_FILES

if [ "$(echo "${FILES_MODIFY[@]}" | grep -xc "$OPEN_API_FILE")" -gt 0 ]
then
    update_openapi_version $DRY_RUN
fi

if [ true = $DRY_RUN ]
then
    echo -e "${ORANGE}warning${RESET}: script is run in dry mode. To apply the changes, run ${GREEN}$0 --run${RESET}"
else
  UPDATED_CRATES="$(find . -name Cargo.toml -exec git diff "$COMMIT_REF" {} + | grep -E "^[\+\-]version = \"[0-9\.]+\"|name = " | tr '\n' ' ' | sed -r "s/ name = \"([a-z\-]+)\" -version = \"([0-9\.]+)\" \+version = \"([0-9\.]+)\" ?/* \1 from \`\2\` to \`\3\`\n/g")"
  if [[ -n $UPDATED_CRATES ]]
  then
    UPDATED_CRATES="\n${UPDATED_CRATES}"
  fi
  UPDATED_PACKAGE_JSONS="$(echo "$PACKAGE_JSON_FILES" | xargs git diff "$COMMIT_REF" | grep -E "^[\+\-] *\"version\": \"[0-9\.]+\"|name\": " | tr '\n' ' ' | sed -r "s/ *\"name\": \"([a-z@\-]*\/)?([a-z\-]+)\", -  \"version\": \"([0-9\.]+)\", \+  \"version\": \"([0-9\.]+)\", ?/* [js] \2 from \`\3\` to \`\4\`\n/g")"
  if [[ -n $UPDATED_PACKAGE_JSONS ]]
  then
    UPDATED_PACKAGE_JSONS="\n${UPDATED_PACKAGE_JSONS}"
  fi

  COMMIT_MESSAGE=$(echo -e "chore: upgrade crate versions${OPEN_API_UPDATE_MESSAGE}\n${UPDATED_CRATES}${UPDATED_PACKAGE_JSONS}${OPEN_API_UPDATE}")

  echo -e "$COMMIT_MESSAGE"

  if [ true = $COMMIT ]
  then
    git add --update $OPEN_API_FILE Cargo.lock ./*/Cargo.toml ./internal/*/Cargo.toml ./mithril-test-lab/*/Cargo.toml
    git add --update ./*/package.json ./*/package-lock.json examples/*/package.json examples/*/package-lock.json
    git commit -m "$COMMIT_MESSAGE"
  fi
fi
