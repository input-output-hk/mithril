#!/bin/bash

set -e

JSON_FILE=$1

if [[ ! -f "$JSON_FILE" ]]; then
    echo "Error: $JSON_FILE not found!"
    exit 1
fi

# Get all unique keys from 'cardano-minimum-version'
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
echo "## Cardano Node Compatibility"
echo ""
echo "$header"
echo "$separator"

# Process each top-level network (mainnet, preprod, preview)
for MITHRIL_NETWORK in $(jq -r 'keys[]' "$JSON_FILE"); do
    # Get all mithril-networks for this top-level network
    jq -r ".\"$MITHRIL_NETWORK\".\"mithril-networks\"[] | keys[]" "$JSON_FILE" | while read -r MITHRIL_NETWORK_NAME; do
        row="| $MITHRIL_NETWORK_NAME"
        
        # For each cardano-minimum-version key, get the value
        for MITHRIL_NODE in $MITHRIL_NODES; do
            version=$(jq -r ".\"$MITHRIL_NETWORK\".\"cardano-minimum-version\".\"$MITHRIL_NODE\" // \"N/A\"" "$JSON_FILE")
            if [[ "$version" != "N/A" ]]; then
                row="$row | Cardano \`$version+\`<sup>(*)</sup>"
            else
                row="$row | N/A"
            fi
        done
        
        row="$row |"
        echo "$row"
    done
done

echo ""
echo "<sup>*</sup>: Up to the latest Cardano node version released at the time of this release."