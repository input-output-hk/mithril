set +a -eu -o pipefail

# -----------------------------------------------------------------------------
# Requirements:
#   - cargo
#   - jq
#
# Exit code:
#   0 => OK
#   1 => At least one version mismatch
# -----------------------------------------------------------------------------
RESET='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'

has_warning=0

metadata_json=$(cargo metadata --format-version 1 --no-deps)

crates_dependencies_to_check=(
    $(echo "${metadata_json}" | jq -r '
        .packages[]
        | select(.name | startswith("mithril-"))
        | .name
    ')
)

for crate_dependency in "${crates_dependencies_to_check[@]}"; do
    # Retrieve actual workspace crate version
    crate_version=$(
        echo "${metadata_json}" \
        | jq -r --arg crate "${crate_dependency}" '
            .packages[]
            | select(.name == $crate)
            | .version
            ' \
        | head -n1
    )

    if [[ -z "${crate_version}" || "${crate_version}" == "null" ]]; then
        echo "WARNING: Cannot find workspace crate version for ${crate_dependency}"
        has_warning=1
        continue
    fi

    echo
    echo -n "Checking that all usages of ${crate_dependency} use version ${crate_version}"

    # Inspect all packages depending on this crate
    mismatches=$(
    echo "${metadata_json}" \
        | jq -r \
        --arg crate "${crate_dependency}" \
        --arg expected "${crate_version}" '
            .packages[]
            | . as $pkg
            | .dependencies[]
            | select(.name == $crate)

            # normalize version requirement (allow minor version)
            | (.req | sub("^\\^"; "")) as $normalized_req

            | select(
                $normalized_req != $expected
                and .req != "*"
            )

            | "\($pkg.name)|\(.req)"
        '
    )

    if [[ -n "${mismatches}" ]]; then
    echo -e "${RED} -> KO${RESET}"
    while IFS='|' read -r package_name found_version; do
        echo -e "${RED}ERROR:${RESET} ${package_name} depends on ${crate_dependency} with version ${RED}${found_version}${RESET}"

        has_warning=1
    done <<< "${mismatches}"
    else
        echo -e "${GREEN} -> OK${RESET}"
    fi
done

echo

if [[ "${has_warning}" -eq 1 ]]; then
    echo "Dependency version mismatches detected."
    exit 1
fi

echo "All dependency versions are consistent."
exit 0