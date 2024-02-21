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
    echo $0
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
    rm "$DATA_DIR/*.json" || true
}

# $1=URL $2=artifact_name
download_data() {
    local -r url=${1:?"No URL given to download_data function."};
    local -r artifact=${2:?"No artifact type given to download_data function."};

    echo "Downloading ${artifact} data."
    wget -O - --quiet "${url}" | jq > "$DATA_DIR/${artifact}.json";
}

# $1=URL $2=artifact_name $3=JSON field
download_artifacts() {
    local -r url=${1:?"No URL given to download_artifacts function."};
    local -r artifact=${2:?"No artifact type given to download_artifacts function."};
    local -r json_field=${3:?"No JSON field given to read from artifact list."};
    local -r download_missing_certificate=${4:-false};
    local -i nb=0

    echo -n "Downloading ${artifact} data: "
    tput sc;

    for field in $(jq -r .[].${json_field} < $DATA_DIR/${artifact}s.json);
    do
        tput rc;
        download_data "${url}/${field}" "${artifact}-${field}"

        if [ true = "$download_missing_certificate" ]; then
            download_missing_certificate $(jq -r .certificate_hash $DATA_DIR/${artifact}-${field}.json);
        fi

        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo " ✅";
}

# Download certificate if a file with the hash does not already exist
# $1=certificate_hash
download_missing_certificate() {
    local -r certificate_hash=${1:?"No certificate hashes given to download_missing_certificate function."};

    if [ ! -e "$DATA_DIR/certificate-${certificate_hash}.json" ]; then
        download_data "${BASE_URL}/certificate/${certificate_hash}" "certificate-${certificate_hash}"
    fi
}

download_certificate_chain() {
    local parent_hash=$(jq -r .[0].hash $DATA_DIR/certificates.json);
    local certificate_hash;
    local -i nb=0

    echo -n "Downloading certificate chain: "
    tput sc;

    until [ -z "$parent_hash" ];
    do
        tput rc;
        certificate_hash=$parent_hash;
        download_data "${BASE_URL}/certificate/${certificate_hash}" "certificate-${certificate_hash}"
        parent_hash=$(jq -r .previous_hash $DATA_DIR/certificate-${certificate_hash}.json);
        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo " ✅";
}

# $@=list of transactions_hashes
download_ctx_proof() {
    local -r ctx_hashes=${@:?"No cardano transaction hashes given to download_ctx_proof function."};
    local -i nb=0

    echo -n "Downloading cardano transaction proof: "
    tput sc;

    for cardano_transaction_hash in $ctx_hashes;
    do
        tput rc;
        download_data "${BASE_URL}/proof/cardano-transaction?transaction_hashes=${cardano_transaction_hash}" "ctx-proof-${cardano_transaction_hash}"
        download_missing_certificate $(jq -r .certificate_hash $DATA_DIR/ctx-proof-${cardano_transaction_hash}.json);

        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo " ✅";
}

# $@=list of transactions_hashes
write_ctx_proof_hashes_list() {
    local -r ctx_hashes=${@:?"No cardano transaction hashes given to write_ctx_proof_hashes_list function."};
    local -i nb=0

    echo -n "Writting cardano transaction proof ids to a file: "
    tput sc;

    echo "[" > $DATA_DIR/ctx-proofs.json

    local separator=" "
    for cardano_transaction_hash in $ctx_hashes;
    do
        tput rc;
        cat >> $DATA_DIR/ctx-proofs.json  <<EOF
$separator { "transaction_hash": "$cardano_transaction_hash" }
EOF

    separator=","

        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo "]" >> $DATA_DIR/ctx-proofs.json

    echo " ✅";
}

# MAIN execution

if [ -z "${1-""}" ]; then display_help "No data directory given to download JSON files."; fi;
if [ -z "${2-""}" ]; then display_help "No Mithril Aggregator URL given."; fi;

declare -r DATA_DIR=$1;
shift
declare -r BASE_URL=$1;
shift
declare -r CARDANO_TRANSACTIONS_HASHES=$@;

echo "-- MITHRIL AGGREGATOR FAKE - DATA IMPORTER --"
echo "data_dir:" $DATA_DIR
echo "base_url:" $BASE_URL
echo "tx_hashes:" $CARDANO_TRANSACTIONS_HASHES
echo

if [ ! -d "$DATA_DIR" ]; then error "Specified directory '${DATA_DIR}' is not a directory."; fi
wget --quiet --server-response --spider $BASE_URL 2>/dev/null || error "Could not reach URL '${BASE_URL}'.";

export DATA_DIR URL;

check_requirements;
clean_directory;

download_data "$BASE_URL/epoch-settings" "epoch-settings"

download_data "$BASE_URL/certificates" "certificates"
download_artifacts "$BASE_URL/certificate" "certificate" "hash"
download_certificate_chain

download_data "$BASE_URL/artifact/snapshots" "snapshots"
download_artifacts "$BASE_URL/artifact/snapshot" "snapshot" "digest" true

download_data "$BASE_URL/artifact/mithril-stake-distributions" "mithril-stake-distributions"
download_artifacts "$BASE_URL/artifact/mithril-stake-distribution" "mithril-stake-distribution" "hash" true

download_data "$BASE_URL/artifact/cardano-transactions"  "ctx-commitments"
download_artifacts "$BASE_URL/artifact/cardano-transaction" "ctx-commitment" "hash"

if [ -n "$CARDANO_TRANSACTIONS_HASHES" ]; then
    download_ctx_proof $CARDANO_TRANSACTIONS_HASHES
    write_ctx_proof_hashes_list $CARDANO_TRANSACTIONS_HASHES
fi
