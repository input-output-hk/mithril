#!/usr/bin/env bash
set +a -eu -o pipefail
#set -vx

check_requirements() {
    which wget >/dev/null ||
        error "It seems 'wget' is not installed or not in the path.";
    which jq >/dev/null ||
        error "It seems 'jq' is not installed or not in the path.";
}

display_help() {
    if [ -n "${1-""}" ]; then echo "ERROR: ${1}"; echo; fi
    echo "HELP"
    echo "$0"
    echo
    echo "Import and save data from a given Mithril Aggregator."
    echo
    echo "Usage: $0 DATA_DIRECTORY URL [CARDANO_TRANSACTIONS_HASHES...]"
    echo
    echo "CARDANO_TRANSACTIONS_HASHES: optional list of cardano transactions hashes"
    echo "  for which proof will be fetched."
    echo
    exit 1;
}

error() {
    echo "ERROR: $1";
    exit 1;
}

clean_directory() {
    echo "Cleaning data directory…"
    rm "${DATA_DIR:?}/"*.json || true
}

# $1=URL $2=artifact_name $3=target_dir (default to $DATA_DIR)
download_data() {
    local -r url=${1:?"No URL given to download_data function."};
    local -r artifact=${2:?"No artifact type given to download_data function."};
    local -r target_dir=${3:-$DATA_DIR}
    local -r target_file="$target_dir/${artifact}.json"

    if [ ! -e "$target_file" ]; then
      mkdir -p "$target_dir"
      wget -O - --quiet "${url}" | jq > "$target_file";
    fi
}

# $1=URL $2=artifact_name $3=JSON field
download_artifacts() {
    local -r url=${1:?"No URL given to download_artifacts function."};
    local -r artifact=${2:?"No artifact type given to download_artifacts function."};
    local -r json_field=${3:?"No JSON field given to read from artifact list."};
    local -r download_missing_certificate=${4:-false};
    local -i nb=0
    local -r artifact_dir="$DATA_DIR/${artifact}"

    echo -n "Downloading ${artifact} data: "
    tput sc;

    for field in $(jq -r ".[].${json_field}" < "$DATA_DIR/${artifact}s-list.json");
    do
        tput rc;
        download_data "${url}/${field}" "${field}" "$artifact_dir"

        if [ true = "$download_missing_certificate" ]; then
            download_missing_certificate "$(jq -r ".certificate_hash" "$DATA_DIR/${artifact}/${field}.json")";
        fi

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo " ✅";
}

# Download certificate if a file with the hash does not already exist
# $1=certificate_hash
download_missing_certificate() {
    local -r certificate_hash=${1:?"No certificate hashes given to download_missing_certificate function."};
    download_data "${BASE_URL}/certificate/${certificate_hash}" "${certificate_hash}" "$DATA_DIR/certificate"
}

download_certificate_chain() {
    local parent_hash="";
    local certificate_hash;
    local -i nb=0

    echo -n "Downloading certificate chain: "
    tput sc;

    parent_hash=$(jq -r ".[0].hash" "$DATA_DIR/certificates-list.json");
    until [ -z "$parent_hash" ];
    do
        tput rc;

        certificate_hash=$parent_hash;
        download_data "${BASE_URL}/certificate/${certificate_hash}" "${certificate_hash}" "$DATA_DIR/certificate"
        parent_hash=$(jq -r .previous_hash "$DATA_DIR/certificate/${certificate_hash}.json");

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo " ✅";
}

# $@=list of transactions_hashes
download_ctx_proof() {
    local -r ctx_hashes=${*:?"No cardano transaction hashes given to download_ctx_proof function."};
    local -i nb=0
    local -r artifact_dir="$DATA_DIR/ctx-proof"

    mkdir -p "$artifact_dir"

    echo -n "Downloading cardano transaction proof: "
    tput sc;

    for cardano_transaction_hash in $ctx_hashes;
    do
        tput rc;
        download_data "${BASE_URL}/proof/cardano-transaction?transaction_hashes=${cardano_transaction_hash}" "${cardano_transaction_hash}" "${artifact_dir}"
        download_missing_certificate "$(jq -r ".certificate_hash" "$DATA_DIR/ctx-proof/${cardano_transaction_hash}.json")";

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo " ✅";
}

# $@=list of transactions_hashes
write_ctx_proof_hashes_list() {
    local -r ctx_hashes=${*:?"No cardano transaction hashes given to write_ctx_proof_hashes_list function."};
    local -i nb=0

    echo -n "Writing cardano transaction proof ids to a file: "
    tput sc;

    echo "[" > "$DATA_DIR/ctx-proofs-list.json"

    local separator=" "
    for cardano_transaction_hash in $ctx_hashes;
    do
        tput rc;
        cat >> "$DATA_DIR/ctx-proofs-list.json"  <<EOF
$separator { "transaction_hash": "$cardano_transaction_hash" }
EOF

    separator=","

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo "]" >> "$DATA_DIR/ctx-proofs-list.json"

    echo " ✅";
}

# Join downloaded artifacts files into a single files
# $1=name of the artifacts to join
join_artifacts_files() {
    local -r name=${1:?"No artifact name given to join_artifacts_files function."};
    local -r src="${DATA_DIR:?}/${name}"
    local -r dest="$DATA_DIR"/"$name"s.json
    local buffer="{}"

    echo "Joining ${name} artifacts into ${dest} …"

    local key=""
    for filename in "$src/"*; do
        key=$(basename "${filename%.*}")
        buffer=$(echo $buffer | jq --slurpfile file "$filename" ". += {\"${key}\": \$file[0]}")
    done

    echo "${buffer}" > "$dest"
    rm -rf "$src"
}

# MAIN execution

if [ -z "${1-""}" ]; then display_help "No data directory given to download JSON files."; fi;
if [ -z "${2-""}" ]; then display_help "No Mithril Aggregator URL given."; fi;

declare -r DATA_DIR=$1;
shift
declare -r BASE_URL=$1;
shift
declare -r CARDANO_TRANSACTIONS_HASHES=$*;

echo "-- MITHRIL AGGREGATOR FAKE - DATA IMPORTER --"
echo "data_dir:" "$DATA_DIR"
echo "base_url:" "$BASE_URL"
echo "tx_hashes:" "$CARDANO_TRANSACTIONS_HASHES"
echo

if [ ! -d "$DATA_DIR" ]; then error "Specified directory '${DATA_DIR}' is not a directory."; fi
wget --quiet --server-response --spider "$BASE_URL" 2>/dev/null || error "Could not reach URL '${BASE_URL}'.";

export DATA_DIR URL;

check_requirements;
clean_directory;

echo "Downloading epoch-settings"
download_data "$BASE_URL/epoch-settings" "epoch-settings"

download_data "$BASE_URL/artifact/snapshots" "snapshots-list"
download_artifacts "$BASE_URL/artifact/snapshot" "snapshot" "digest" true

download_data "$BASE_URL/artifact/mithril-stake-distributions" "mithril-stake-distributions-list"
download_artifacts "$BASE_URL/artifact/mithril-stake-distribution" "mithril-stake-distribution" "hash" true

download_data "$BASE_URL/artifact/cardano-stake-distributions" "cardano-stake-distributions-list"
download_artifacts "$BASE_URL/artifact/cardano-stake-distribution" "cardano-stake-distribution" "hash" true

download_data "$BASE_URL/artifact/cardano-database" "cardano-databases-list"
download_artifacts "$BASE_URL/artifact/cardano-database" "cardano-database" "hash" true

download_data "$BASE_URL/artifact/cardano-transactions"  "ctx-snapshots-list"
download_artifacts "$BASE_URL/artifact/cardano-transaction" "ctx-snapshot" "hash"

if [ -n "$CARDANO_TRANSACTIONS_HASHES" ]; then
    download_ctx_proof "$CARDANO_TRANSACTIONS_HASHES"
    write_ctx_proof_hashes_list "$CARDANO_TRANSACTIONS_HASHES"
    join_artifacts_files "ctx-proof"
fi

# Download certificates last to ensure that we take in account the new certificates that were signed while this script run.
#
# else there's an edge case were the list have less than 20 items but we downloaded more individual certificates than that
# (20 is the max number of items for a list endpoint of the aggregator).
download_data "$BASE_URL/certificates" "certificates-list"
download_artifacts "$BASE_URL/certificate" "certificate" "hash"
download_certificate_chain

for artifact_type in "snapshot" "mithril-stake-distribution" "ctx-snapshot" "cardano-stake-distribution" "cardano-database" "certificate"; do
  join_artifacts_files "$artifact_type"
done
