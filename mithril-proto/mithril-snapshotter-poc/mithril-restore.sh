#!/bin/bash -v

# Use Args
SNAPSHOT_FILE=$1
if [ "$SNAPSHOT_FILE" = "" ]; then
    SNAPSHOT_FILE=snapshot.tar.gz
fi
RESTORE_DIR=$2
if [ "$RESTORE_DIR" = "" ]; then
    RESTORE_DIR="mithril-restore"
fi

# Launch
SECONDS=0
echo "Mithril restore";
echo "";
echo ">> SNAPSHOT: $SNAPSHOT_FILE";
echo ">> RESTORE: $RESTORE_DIR";
echo "";

# Print elapsed duration
ElapsedDuration () {
    echo ">> Duration: ${SECONDS}s";
    SECONDS=0
}

# Compute digest of directory
DirectoryDigest () {
    TMP_FILE=.tmp.txt
    rm -f $TMP_FILE
    find $1 -type f ! -name '.DS_Store' -print | sort -n | xargs -I '{}' openssl dgst -sha256 '{}' | awk '{print $2}' >> $TMP_FILE;
    DIGEST=$(cat $TMP_FILE | openssl dgst -sha256 | awk '{print $2}')
    rm $TMP_FILE
    echo $DIGEST
}

# Clean existing dir
rm -rf $RESTORE_DIR;

# Restore snapshot
echo ">> Restore snapshot archive to $RESTORE_DIR";
mkdir -p $RESTORE_DIR/db
tar -xzvf $SNAPSHOT_FILE -C $RESTORE_DIR/db
ElapsedDuration;
echo "";

# Restored list files
echo ">> Restored files list";
find $RESTORE_DIR -type f -print | sort -n ;
echo "";

# Restored dir size
echo ">> Restored files size";
du -h $RESTORE_DIR;
echo "";

# Restored files digest
echo ">> Restored files digest";
DIGEST_RESTORED=$(DirectoryDigest $RESTORE_DIR)
echo ">> Digest SHA256 ($RESTORE_DIR): $DIGEST_RESTORED";
ElapsedDuration;
echo "";

