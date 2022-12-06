#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi

# Check if all env vars are set
if [ -z "${PROCEDURE_FILE_PATH}" ]; then
    echo Missing environment variable: PROCEDURE_FILE_PATH
    exit 1
fi

if [ -z "${GPG_SECRET_KEY}" ]; then
    echo Missing environment variable: GPG_SECRET_KEY
    exit 1
fi

# Create a procedure to verify the checksum and the GPG signature for a downloaded asset from the package
main() {
    PROCEDURE_FILE_PATH=$1
    GPG_SECRET_KEY=$2
    mkdir gpghome
    chmod 700 gpghome
    echo "$GPG_SECRET_KEY" | gpg --homedir gpghome --batch --import
    gpg --homedir gpghome --list-secret-keys
    GPG_FINGERPRINT=$(gpg --homedir gpghome --fingerprint "mithril@iohk.io" | head -n 2 | tail -n 1 | xargs)
    cat >> $PROCEDURE_FILE_PATH << EOF

## Verify the authenticity of a downloaded asset
<details><summary>Detailed procedure to verify an asset</summary>
<p>

* **Step 1**: Identify the downloaded asset on your computer ***YOUR_ASSET_FILE***
* **Step 2**: Download the signed checksum file from this link [CHECKSUM.asc](./CHECKSUM.asc) and save it in the same folder as the asset
* **Step 3**: In your terminal, go to the asset folder by running:
\`\`\`
cd ***YOUR_ASSET_FOLDER***
\`\`\`
* **Step 4**: Then verify the checksum of the asset by running:
\`\`\`
sha256sum -c ./CHECKSUM.asc 2>/dev/null | grep ***YOUR_ASSET_FILE***
\`\`\`
You must see:
\`\`\`
./***YOUR_ASSET_FILE***: OK
\`\`\`
* **Step 5**: Download the public key file from this link [gpg-public.key](./gpg-public.key) and save it in the same folder as the asset
* **Step 6**: Then import the GPG public key:
\`\`\`
gpg --import ./gpg-public.key
\`\`\`
You must see something like:
\`\`\`
gpg: key : public key "Input Output / Mithril <mithril@iohk.io>" imported
gpg: Total number processed: 1
gpg:               imported: 1
\`\`\`
* **Step 7**: Then verify the GPG signature of the checksum file:
\`\`\`
gpg --verify ./gpg-public.key ./CHECKSUM.asc
\`\`\`
You must see something like:
\`\`\`
gpg: Signature made Mon 05 Dec 2022 04:53:54 PM CET
gpg:                using RSA key 35EDE9D47BBA62A2F388E655899ACD26B8BCA0D2
gpg: Good signature from "Input Output / Mithril <mithril@iohk.io>" [unknown]
gpg: WARNING: This key is not certified with a trusted signature!
gpg:          There is no indication that the signature belongs to the owner.
Primary key fingerprint: 35ED E9D4 7BBA 62A2 F388  E655 899A CD26 B8BC A0D2
\`\`\`
The signature is valid if and only if:
- there is a line with \`gpg: Good signature from "Input Output / Mithril <mithril@iohk.io>"\`
- there is a line with \`Primary key fingerprint: ${GPG_FINGERPRINT}\`
* **Step 8**:
If you successfully validated all the steps of this process, then you have successfully verified the authenticity of the asset :heavy_check_mark:
If not, contact us at [mithril@iohk.io] and let us know of the outcome of your run of this process :warning:

</p>
</details>

EOF

    cat $PROCEDURE_FILE_PATH
    rm -rf gpghome
}

main "$PROCEDURE_FILE_PATH" "$GPG_SECRET_KEY" 
