#!/bin/bash
SIGNER_NODE=$1

exec_sql_signer() {
  sqlite3 data/preview/$1/mithril/stores/signer.sqlite3 "$2"
}

fix_signer() {
  LAST_EPOCH=$(exec_sql_signer $1 "SELECT MAX(key) FROM $2;")
  LAST_VALUE=$(exec_sql_signer $1 "SELECT value FROM $2 WHERE key = $LAST_EPOCH;")

  CREATE_EPOCH=$LAST_EPOCH
  CREATE_EPOCH=$(expr $CREATE_EPOCH - $3)
  CREATE_EPOCH_HASH=$(echo -n $CREATE_EPOCH | sha256sum | awk '{print $1}')
  exec_sql_signer $1 "INSERT OR IGNORE INTO $2(key_hash, key, value) VALUES('$CREATE_EPOCH_HASH','$CREATE_EPOCH','$LAST_VALUE');"
  exec_sql_signer $1 "SELECT * FROM $2 WHERE key = $CREATE_EPOCH;"
}

fix_signer $SIGNER_NODE stake 1
fix_signer $SIGNER_NODE stake 2
fix_signer $SIGNER_NODE protocol_initializer 1
fix_signer $SIGNER_NODE protocol_initializer 2

