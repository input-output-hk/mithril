#! /bin/bash

set -e

TOTAL_RELEASES=$1
ASSETS_DIRECTORY="./mithril-binaries"
TAG_FILE=$ASSETS_DIRECTORY/tags.json

echo ">> Retrieving artifacts for last ${TOTAL_RELEASES} releases and unstable"
mkdir -p $ASSETS_DIRECTORY
gh api /repos/input-output-hk/mithril/releases | jq -r --argjson TOTAL $TOTAL_RELEASES '[.[] | select(.prerelease == false)] | [.[:$TOTAL][].tag_name]' >> $TAG_FILE

TAG_NAMES=$(cat $TAG_FILE | jq -r '.[]' | tr '\n' ' ')
for TAG_NAME in unstable $TAG_NAMES
do
    echo ">>>> Retrieving artifacts for release ${TAG_NAME}"
    ARCHIVE_DIRECTORY="./mithril-binaries/$TAG_NAME"
    mkdir -p $ARCHIVE_DIRECTORY
    gh release download --repo input-output-hk/mithril $TAG_NAME -O mithril-archive --pattern "mithril-$TAG_NAME*-linux-x64.tar.gz"
    tar xzf mithril-archive -C $ARCHIVE_DIRECTORY mithril-aggregator mithril-signer mithril-client mithril-relay
    rm mithril-archive
done
