#!/bin/bash
NETWORK=$1
AGGREGATOR_NODE=$2
GENESIS_KEY_NEW=$3
GENESIS_VALUE_NEW=$4

exec_sql_aggregator() {
  sqlite3 data/$1/$2/mithril/stores/aggregator.sqlite3 "$3"
}

fix_genesis_certificate() {
  GENESIS_KEY_NEW='"'$3'"'
  GENESIS_VALUE_NEW=$4
  GENESIS_KEY_NEW_HASH=$(echo -n $GENESIS_KEY_NEW | sha256sum | awk '{print $1}')
  exec_sql_aggregator $1 $2 "SELECT * FROM certificate WHERE json_extract(value, '$.genesis_signature') <> '';"
  exec_sql_aggregator $1 $2 "DELETE FROM certificate;"
  exec_sql_aggregator $1 $2 "INSERT OR IGNORE INTO certificate(key_hash, key, value) VALUES('$GENESIS_KEY_NEW_HASH','$GENESIS_KEY_NEW','$GENESIS_VALUE_NEW');"
  exec_sql_aggregator $1 $2 "SELECT * FROM certificate WHERE json_extract(value, '$.genesis_signature') <> '';"
}

fix_genesis_certificate $NETWORK  $AGGREGATOR_NODE $GENESIS_KEY_NEW $GENESIS_VALUE_NEW