#!/usr/bin/env bash
set +a -eu -o pipefail
#set -vx

check_requirements() {
    which wget >/dev/null ||
        error "It seems 'wget' is not installed or not in the path.";
    which jq >/dev/null ||
        error "It seems 'jq' is not installed or not in the path.";
    if [ "${BASH_VERSINFO[0]}" -lt 5 ]; then
      local homebrew_hint && [ "$(uname -s)" = "Darwin" ] && homebrew_hint=" You can install it using homebrew: 'brew install bash'"
      error "Bash 5+ is required, but found version ${BASH_VERSION}.${homebrew_hint:-}"
    fi
}

parse_arguments() {
    while [[ "${1:-}" == -* && ! "${1:-}" == "--" ]]; do case $1 in
      -h | --help ) display_help ;;
      -d | --data-dir ) shift; declare -gr DATA_DIR=${1:-} ;;
      -u | --base-url ) shift; declare -gr BASE_URL=${1:-} ;;
      -t | --tx-hashes ) shift; declare CARDANO_TRANSACTIONS_HASHES=${1:-} ;;
      -b | --block-hashes ) shift; declare CARDANO_BLOCKS_HASHES=${1:-} ;;
    esac; shift; done
    if [[ "${1:-}" == '--' ]]; then shift; fi

    if [ -z "${DATA_DIR:-""}" ]; then display_help "No data directory given (-d/--data-dir)."; fi
    if [ -z "${BASE_URL:-""}" ]; then display_help "No Mithril Aggregator URL given (-u/--base-url)."; fi

    declare -gr CARDANO_TRANSACTIONS_HASHES=${CARDANO_TRANSACTIONS_HASHES:-""}
    declare -gr CARDANO_BLOCKS_HASHES=${CARDANO_BLOCKS_HASHES:-""}
}

display_help() {
    if [ -n "${1:-""}" ]; then echo "ERROR: ${1}"; echo; fi
    echo "HELP"
    echo "$0"
    echo
    echo "Import and save data from a given Mithril Aggregator."
    echo
    echo "Usage: $0 -d DATA_DIRECTORY -u URL [-t CARDANO_TRANSACTIONS_HASHES] [-b CARDANO_BLOCKS_HASHES]"
    echo
    echo "Options:"
    echo "  -d, --data-dir    Directory where the downloaded data will be saved."
    echo "  -u, --base-url    URL of the Mithril Aggregator."
    echo "  -t, --tx-hashes   Optional comma-separated list of Cardano transaction hashes"
    echo "                      for which a proof will be fetched."
    echo "  -b, --block-hashes  Optional comma-separated list of Cardano block hashes"
    echo "                      for which a proof will be fetched."
    echo "  -h, --help        Print help"
    echo

    # exit 1 if called with an error message, 0 otherwise
    exit ${1:+1}
}

error() {
    echo "ERROR: $1";
    exit 1;
}

# run tput or noop when the terminal is not interactive (such as in CI or when redirecting output)
_tput() { [ -t 1 ] && tput "$@" || true; }

clean_directory() {
    echo "Cleaning data directory…"
    rm "${DATA_DIR:?}/"*.json || true
}

# $1=URL $2=artifact_name $3=target_dir (default to $DATA_DIR)
download_data() {
    local -r url=${1:?"No URL given to download_data function."};
    local -r artifact=${2:?"No artifact name given to download_data function."};
    local -r target_dir=${3:-$DATA_DIR}
    local -r target_file="$target_dir/${artifact}.json"

    if [ ! -e "$target_file" ]; then
      mkdir -p "$target_dir"
      wget -O - --quiet "${url}" | jq > "$target_file";
    fi
}

# $1=URL $2=artifact_name $3=JSON field $4=trigger associated certificate download if missing (default false)
download_artifacts() {
    local -r url=${1:?"No URL given to download_artifacts function."};
    local -r artifact=${2:?"No artifact name given to download_artifacts function."};
    local -r json_field=${3:?"No JSON field given to read from artifact list."};
    local -r download_missing_certificate=${4:-false};
    local -i nb=0
    local -r artifact_dir="$DATA_DIR/${artifact}"

    echo -n "Downloading ${artifact} data: "
    _tput sc;

    for field in $(jq -r ".[].${json_field}" < "$DATA_DIR/${artifact}s-list.json");
    do
        _tput rc;
        download_data "${url}/${field}" "${field}" "$artifact_dir"

        if [ "$download_missing_certificate" = true ]; then
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
    _tput sc;

    parent_hash=$(jq -r ".[0].hash" "$DATA_DIR/certificates-list.json");
    until [ -z "$parent_hash" ];
    do
        _tput rc;

        certificate_hash=$parent_hash;
        download_data "${BASE_URL}/certificate/${certificate_hash}" "${certificate_hash}" "$DATA_DIR/certificate"
        parent_hash=$(jq -r .previous_hash "$DATA_DIR/certificate/${certificate_hash}.json");

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo " ✅";
}

# $1=proof_subject (block or transaction) $2=URL $3=artifact_name $*:4=list of hashes
download_proof() {
    local -r proof_subject=${1:?"No proof subject given to download_proof function, expect either 'block' or 'transaction'."};
    local -r url=${2:?"No URL given to download_proof function."};
    local -r artifact=${3:?"No artifact name given to download_proof function."};
    local -r hashes=${*:4};
    local -i nb=0
    local -r artifact_dir="$DATA_DIR/$artifact"

    if [ -z "$hashes" ]; then error "No cardano ${proof_subject} hashes given to download_proof function."; fi

    mkdir -p "$artifact_dir"

    echo -n "Downloading cardano ${proof_subject} proof: "
    _tput sc;

    for cardano_hash in $hashes;
    do
        _tput rc;
        download_data "${url}?${proof_subject}_hashes=${cardano_hash}" "${cardano_hash}" "${artifact_dir}"
        download_missing_certificate "$(jq -r ".certificate_hash" "$DATA_DIR/${artifact}/${cardano_hash}.json")";

        ((nb=nb+1))
        echo -n "$nb   "
    done
    echo " ✅";
}

# $1=proof_subject (block or transaction) $2=artifact_name $*:3=list of hashes
write_proof_hashes_list() {
    local -r proof_subject=${1:?"No proof subject given to write_proof_hashes_list function, expect either 'block' or 'transaction'."};
    local -r artifact=${2:?"No artifact name given to write_proof_hashes_list function."};
    local -r hashes=${*:3};
    local -i nb=0

    if [ -z "$hashes" ]; then error "No cardano ${proof_subject} hashes given to write_proof_hashes_list function."; fi

    echo -n "Writing cardano ${proof_subject} proof ids to a file: "

    # Intentional unquoted variable here: ${hashes} must expand as separate positional args
    jq -n --arg key "${proof_subject}_hash" '[$ARGS.positional[] | {($key): .}]' --args ${hashes} \
        > "$DATA_DIR/${artifact}-list.json"

    echo " ✅";
}

# Join downloaded artifacts files into a single files
# $1=name of the artifacts to join $2=optional target file name (without extension)
join_artifacts_files() {
    local -r name=${1:?"No artifact name given to join_artifacts_files function."};
    local -r target_name=${2:-"${name}s"};
    local -r src="${DATA_DIR:?}/${name}"
    local -r dest="$DATA_DIR"/"${target_name}".json

    echo "Joining ${name} artifacts into ${dest} …"
    jq -n 'reduce inputs as $f ({}; . + {(input_filename | gsub(".*/|[.]json$";"")): $f})' "$src/"*.json > "$dest"

    rm -rf "$src"
}

join_all_artifacts_files() {
    for artifact_type in \
        "snapshot" \
        "mithril-stake-distribution" \
        "ctx-snapshot" \
        "cardano-blocks-txs-snapshot" \
        "cardano-stake-distribution" \
        "cardano-database" \
        "certificate"
    do
        join_artifacts_files "$artifact_type"
    done
}

# ──────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────
check_requirements
parse_arguments "$@"

echo "-- MITHRIL AGGREGATOR FAKE - DATA IMPORTER --"
echo "data_dir:" "$DATA_DIR"
echo "base_url:" "$BASE_URL"
echo "tx_hashes:" "${CARDANO_TRANSACTIONS_HASHES}"
echo "block_hashes:" "${CARDANO_BLOCKS_HASHES}"
echo

if [ ! -d "$DATA_DIR" ]; then error "Specified directory '${DATA_DIR}' is not a directory."; fi
wget --quiet --server-response --spider "$BASE_URL" 2>/dev/null || error "Could not reach URL '${BASE_URL}'."

clean_directory

echo "Downloading aggregator status"
download_data "$BASE_URL/status" "status"

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

download_data "$BASE_URL/artifact/cardano-transactions" "ctx-snapshots-list"
download_artifacts "$BASE_URL/artifact/cardano-transaction" "ctx-snapshot" "hash"

download_data "$BASE_URL/artifact/cardano-blocks-transactions" "cardano-blocks-txs-snapshots-list"
download_artifacts "$BASE_URL/artifact/cardano-blocks-transactions" "cardano-blocks-txs-snapshot" "hash"

if [ -n "$CARDANO_TRANSACTIONS_HASHES" ]; then
    readarray -d "," -t TRANSACTIONS_HASHES <<< "$CARDANO_TRANSACTIONS_HASHES"

    download_proof "transaction" "${BASE_URL}/proof/cardano-transaction" "ctx-proof" "${TRANSACTIONS_HASHES[@]}"
    write_proof_hashes_list "transaction" "ctx-proofs" "${TRANSACTIONS_HASHES[@]}"
    join_artifacts_files "ctx-proof"

    download_proof "transaction" "${BASE_URL}/proof/v2/cardano-transaction" "ctx-proof-v2" "${TRANSACTIONS_HASHES[@]}"
    write_proof_hashes_list "transaction" "ctx-proofs-v2" "${TRANSACTIONS_HASHES[@]}"
    join_artifacts_files "ctx-proof-v2" "ctx-proofs-v2"
fi

if [ -n "$CARDANO_BLOCKS_HASHES" ]; then
    readarray -d "," -t BLOCK_HASHES <<< "$CARDANO_BLOCKS_HASHES"

    download_proof "block" "${BASE_URL}/proof/v2/cardano-block" "cblk-proof" "${BLOCK_HASHES[@]}"
    write_proof_hashes_list "block" "cblk-proofs" "${BLOCK_HASHES[@]}"
    join_artifacts_files "cblk-proof"
fi

# Download certificates last to ensure that we take in account the new certificates that were signed while this script run.
#
# else there's an edge case were the list have less than 20 items but we downloaded more individual certificates than that
# (20 is the max number of items for a list endpoint of the aggregator).
download_data "$BASE_URL/certificates" "certificates-list"
download_artifacts "$BASE_URL/certificate" "certificate" "hash"
download_certificate_chain

join_all_artifacts_files
