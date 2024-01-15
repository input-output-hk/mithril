#!/usr/bin/env bash
set +a -eu -o pipefail
#set -v

error() {
    echo $1;
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
    local -i nb=1

    echo -n "Downloading ${artifact} data: "
    tput sc;

    for field in $(jq -r .[].${json_field} < $DATA_DIR/${artifact}s.json);
    do
        tput rc;
        wget -O $DATA_DIR/${artifact}-${field}.json --quiet "${url}/${field}"
        let "nb=nb+1"
        echo -n "$nb   "
    done
    echo
}

download_certificate_chain() {
    echo -n "Downloading certificate chain: "
    tput sc;
    local parent_hash=$(jq -r .[0].hash $DATA_DIR/certificates.json);
    local certificate_hash;

    until [ -z "$parent_hash" ];
    do
        certificate_hash=$parent_hash;
        wget -O $DATA_DIR/certificate-${certificate_hash}.json --quiet "${BASE_URL}/certificate/${certificate_hash}";
        parent_hash=$(jq -r .previous_hash $DATA_DIR/certificate-${certificate_hash}.json);
        echo -n "${parent_hash} ";
        tput rc;
    done

    echo "ok                                                                ";
}

# MAIN execution

declare -r DATA_DIR=${1:?"No data directory given to download JSON files."};
declare -r BASE_URL=${2:?"No Mithril Aggregator URL given."};

if [ ! -d "$DATA_DIR" ]; then error "Specified directory '${DATA_DIR}' is not a directory."; fi
wget --quiet --server-response --spider $BASE_URL || error "Could not reach URL '${BASE_URL}'.";

export DATA_DIR URL;

clean_directory;

download_list $BASE_URL/certificates certificates
download_certificate_chain

download_list $BASE_URL/artifact/snapshots snapshots
download_artifacts $BASE_URL/artifact/snapshot snapshot digest

download_list $BASE_URL/artifact/mithril-stake-distributions  mithril-stake-distributions
download_artifacts $BASE_URL/artifact/mithril-stake-distribution mithril-stake-distribution "hash"
