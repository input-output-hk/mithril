#!/usr/bin/env bash
set +a -eu -o pipefail
#set -vx

check_requirements() {
    which wget >/dev/null ||
        error "It seems 'wget' is not installed or not in the path.";
}

display_help() {
    if [ -n "${1-""}" ]; then echo "ERROR: ${1}"; echo; fi
    echo "HELP"
    echo $0
    echo
    echo "Import and save data from a given Mithril Aggregator."
    echo
    echo "Usage: $0 DATA_DIRECTORY URL"
    echo
    exit 1;
}

error() {
    echo "ERROR: $1";
    exit 1;
}

clean_directory() {
    echo "Cleaning data directory…"
    rm $DATA_DIR/*.json || true
}

# $1=URL $2=artifact_name
download_list() {
    local -r url=${1:?"No URL given to download_list function."};
    local -r artifact=${2:?"No artifact type given to download_list function."};

    echo "Downloading ${artifact} list."
    wget -O $DATA_DIR/${artifact}.json --quiet "${url}"
}

# $1=URL $2=artifact_name $3=JSON field
download_artifacts() {
    local -r url=${1:?"No URL given to download_list function."};
    local -r artifact=${2:?"No artifact type given to download_list function."};
    local -r json_field=${3:?"No JSON field given to read from artifact list."};
    local -i nb=0

    echo -n "Downloading ${artifact} data: "
    tput sc;

    for field in $(jq -r .[].${json_field} < $DATA_DIR/${artifact}s.json);
    do
        tput rc;
        wget -O $DATA_DIR/${artifact}-${field}.json --quiet "${url}/${field}"
        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo " ✅";
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
        wget -O $DATA_DIR/certificate-${certificate_hash}.json --quiet "${BASE_URL}/certificate/${certificate_hash}";
        parent_hash=$(jq -r .previous_hash $DATA_DIR/certificate-${certificate_hash}.json);
        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo " ✅";
}

# MAIN execution

if [ -z "${1-""}" ]; then display_help "No data directory given to download JSON files."; fi;
if [ -z "${2-""}" ]; then display_help "No Mithril Aggregator URL given."; fi;

declare -r DATA_DIR=$1;
declare -r BASE_URL=$2;

if [ ! -d "$DATA_DIR" ]; then error "Specified directory '${DATA_DIR}' is not a directory."; fi
wget --quiet --server-response --spider $BASE_URL 2>/dev/null || error "Could not reach URL '${BASE_URL}'.";

export DATA_DIR URL;

check_requirements;
clean_directory;

download_list "$BASE_URL/certificates" "certificates"
download_artifacts "$BASE_URL/certificate" "certificate" "hash"
download_certificate_chain

download_list "$BASE_URL/artifact/snapshots" "snapshots"
download_artifacts "$BASE_URL/artifact/snapshot" "snapshot" "digest"

download_list "$BASE_URL/artifact/mithril-stake-distributions"  "mithril-stake-distributions"
download_artifacts "$BASE_URL/artifact/mithril-stake-distribution" "mithril-stake-distribution" "hash"

