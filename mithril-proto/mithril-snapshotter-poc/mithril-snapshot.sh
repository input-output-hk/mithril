#!/bin/bash -v

# Use Args
SNAPSHOT_MODE=$1 #full or light
INPUT_DIR=$2
if [ "$INPUT_DIR" = "" ]; then
    echo "You must specify an input directory!";
    exit;
fi
SNAPSHOT_DIR="$INPUT_DIR.snapshot"
RESTORE_DIR=$3
if [ "$RESTORE_DIR" = "" ]; then
    RESTORE_DIR="$INPUT_DIR.restore"
fi

# Launch
SECONDS=0
echo "Mithril Snapshot in '$SNAPSHOT_MODE' mode";
echo "";
echo ">> SOURCE: $INPUT_DIR";
echo ">> SNAPSHOT: $SNAPSHOT_DIR";
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
rm -rf $SNAPSHOT_DIR;
rm -rf $RESTORE_DIR;

# List input files
echo ">> Input files list";
find $INPUT_DIR -type f -print;
echo "";

# Input size
echo ">> Input files size";
du -h $INPUT_DIR;
echo "";

# Snapshot "immutable"
echo ">> Copy immutable files to $SNAPSHOT_DIR/src/db/immutable";
mkdir -p $SNAPSHOT_DIR/src/db/immutable;
find $INPUT_DIR -type f -print | sort -n | grep -i immutable | xargs -I '{}' cp '{}' $SNAPSHOT_DIR/src/db/immutable;
ElapsedDuration;
echo "";

# Snapshot "protocolMagicId"
echo ">> Copy protocolMagicId file to $SNAPSHOT_DIR/src/db";
cp $INPUT_DIR/db/protocolMagicId $SNAPSHOT_DIR/src/db;
ElapsedDuration;
echo "";

# Snapshot "ledger"
if [ "$SNAPSHOT_MODE" = "full" ]; then
    echo ">> Copy ledger state latest file to $SNAPSHOT_DIR/src/db/ledger";
    mkdir -p $SNAPSHOT_DIR/src/db/ledger;
    echo "find $INPUT_DIR -type f ! -name '.DS_Store' -print | sort -n | grep -i ledger | head -n 1 | xargs -I '{}' cp '{}' $SNAPSHOT_DIR/src/db/ledger/;"
    find $INPUT_DIR -type f ! -name '.DS_Store' -print | sort -n | grep -i ledger | head -n 1 | xargs -I '{}' cp '{}' $SNAPSHOT_DIR/src/db/ledger/;
    ElapsedDuration;
    echo "";
fi

# List snapshot src files
echo ">> Snapshot src files list";
find $SNAPSHOT_DIR/src/db -type f -print;
echo "";

# Snapshot src size
echo ">> Snapshot src files size";
du -h $SNAPSHOT_DIR/src/db;
echo "";

# Create snapshot archive 
echo ">> Create snapshot archive in $SNAPSHOT_DIR/archive/snapshot.tar.gz";
mkdir -p $SNAPSHOT_DIR/archive;
tar -czvf $SNAPSHOT_DIR/archive/snapshot.tar.gz -C $SNAPSHOT_DIR/src/db/ .;
ElapsedDuration;
echo "";

# Snapshot files list
echo ">> Snapshot archive files list";
tar -ztvf $SNAPSHOT_DIR/archive/snapshot.tar.gz
echo "";

# Snapshot archive size
echo ">> Snapshot archive file size";
du -h $SNAPSHOT_DIR/archive;
echo "";

# Restore snapshot
echo ">> Restore snapshot archive to $RESTORE_DIR";
mkdir -p $RESTORE_DIR/db
tar -xzvf $SNAPSHOT_DIR/archive/snapshot.tar.gz -C $RESTORE_DIR/db
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

# Snapshot src files digest
echo ">> Snapshot src files digest";
DIGEST_SRC=$(DirectoryDigest $SNAPSHOT_DIR/src/db)
echo ">> Digest SHA256 ($SNAPSHOT_DIR/src/db): $DIGEST_SRC";
ElapsedDuration;
echo "";

# Restored files digest
echo ">> Restored files digest";
DIGEST_RESTORED=$(DirectoryDigest $RESTORE_DIR)
echo ">> Digest SHA256 ($RESTORE_DIR): $DIGEST_RESTORED";
ElapsedDuration;
echo "";

# Check digests
if [ "$DIGEST_SRC" = "$DIGEST_RESTORED" ]; then
    echo ">> Congrats the digests are the same!";
else
    echo ">> Oops the digests are different!";
fi