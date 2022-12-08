#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi

# Check if all env vars are set
if [ -z "${GPG_SECRET_KEY}" ]; then
    echo Missing environment variable: GPG_SECRET_KEY
    exit 1
fi

# Create a sha256 checksum manifest of the files of the package and sign it with GPG
main() {
    GPG_SECRET_KEY=$1
    mkdir gpghome
    chmod 700 gpghome
    echo "$GPG_SECRET_KEY" | gpg --homedir gpghome --batch --import
    gpg --homedir gpghome --list-secret-keys
    gpg --homedir gpghome --export --armor mithril@iohk.io > ./package/gpg-public.key
    cd ./package
    find . -type f -print | grep -v CHECKSUM | sort -n | xargs -I '{}' sha256sum '{}' > ./CHECKSUM
    gpg --homedir ../gpghome --clear-sign ./CHECKSUM
    rm -f ./CHECKSUM
    sha256sum -c ./CHECKSUM.asc
    gpg --homedir ../gpghome --verify ./CHECKSUM.asc
    cd ..
    rm -rf gpghome
}

main "$GPG_SECRET_KEY"