#!/bin/bash
NETWORK=$1
SIGNER_NODE=$2

exec_sql_signer() {
  sqlite3 data/$1/$2/mithril/stores/signer.sqlite3 "$3"
}

fix_table_signer() {
  LAST_EPOCH=$(exec_sql_signer $1 $2 "SELECT MAX(key) FROM $3;")
  LAST_VALUE=$(exec_sql_signer $1 $2 "SELECT value FROM $3 WHERE key = $LAST_EPOCH;")

  CREATE_EPOCH=$LAST_EPOCH
  CREATE_EPOCH=$(expr $CREATE_EPOCH - $4)
  CREATE_EPOCH_HASH=$(echo -n $CREATE_EPOCH | sha256sum | awk '{print $1}')
  exec_sql_signer $1 $2 "INSERT OR IGNORE INTO $3(key_hash, key, value) VALUES('$CREATE_EPOCH_HASH','$CREATE_EPOCH','$LAST_VALUE');"
  exec_sql_signer $1 $2 "SELECT * FROM $3 WHERE key = $CREATE_EPOCH;"
}

fix_table_signer $NETWORK $SIGNER_NODE stake 1
fix_table_signer $NETWORK $SIGNER_NODE stake 2
fix_table_signer $NETWORK $SIGNER_NODE stake 3
fix_table_signer $NETWORK $SIGNER_NODE protocol_initializer 1
fix_table_signer $NETWORK $SIGNER_NODE protocol_initializer 2
fix_table_signer $NETWORK $SIGNER_NODE protocol_initializer 3

