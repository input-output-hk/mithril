#!/bin/bash

set -e

JSON_FILE=$1

if [[ ! -f "$JSON_FILE" ]]; then
    echo "Error: $JSON_FILE not found!"
    exit 1
fi

# Derive the Mithril node columns from the 'cardano-minimum-version' keys, so the
# DMQ table stays aligned with the Cardano compatibility table
MITHRIL_NODES=$(jq -r '[.[] | .["cardano-minimum-version"] | keys[]] | unique | .[]' "$JSON_FILE")

# Create header of the markdown table
header="| Network"
separator="|----------"

for key in $MITHRIL_NODES; do
    title=$(echo "$key" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
    header="$header | $title"
    separator="$separator |:-------------:"
done

header="$header |"
separator="$separator |"

echo ""
echo "## DMQ Node Compatibility"
echo ""
echo "$header"
echo "$separator"

# Process each top-level network (mainnet, preprod, preview)
for MITHRIL_NETWORK in $(jq -r 'keys[]' "$JSON_FILE"); do
    DMQ_VERSION=$(jq -r ".\"$MITHRIL_NETWORK\".\"dmq-minimum-version\" // \"N/A\"" "$JSON_FILE")

    # Get all mithril-networks for this top-level network
    jq -r ".\"$MITHRIL_NETWORK\".\"mithril-networks\"[] | keys[]" "$JSON_FILE" | while read -r MITHRIL_NETWORK_NAME; do
        row="| $MITHRIL_NETWORK_NAME"

        # Repeat the DMQ minimum version for each mithril node
        for _mithril_node in $MITHRIL_NODES; do
            if [[ "$DMQ_VERSION" != "N/A" ]]; then
                row="$row | DMQ \`$DMQ_VERSION+\`<sup>(*)</sup>"
            else
                row="$row | N/A"
            fi
        done

        row="$row |"
        echo "$row"
    done
done

echo ""
echo "<sup>*</sup>: Up to the latest DMQ node version released at the time of this release."
