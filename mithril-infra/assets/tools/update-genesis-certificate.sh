#!/bin/bash
AGGREGATOR_NODE=$1
GENESIS_KEY_NEW=$2
GENESIS_VALUE_NEW=$3

exec_sql_aggregator() {
  sqlite3 data/preview/$1/mithril/stores/aggregator.sqlite3 "$2"
}

fix_genesis_certificate() {
  GENESIS_KEY_NEW='"'$2'"'
  GENESIS_VALUE_NEW=$3
  GENESIS_KEY_NEW_HASH=$(echo -n $GENESIS_KEY_NEW | sha256sum | awk '{print $1}')
  exec_sql_aggregator $1 "SELECT * FROM certificate WHERE json_extract(value, '$.genesis_signature') <> '';"
  exec_sql_aggregator $1 "DELETE FROM certificate;"
  exec_sql_aggregator $1 "INSERT OR IGNORE INTO certificate(key_hash, key, value) VALUES('$GENESIS_KEY_NEW_HASH','$GENESIS_KEY_NEW','$GENESIS_VALUE_NEW');"
  exec_sql_aggregator $1 "SELECT * FROM certificate WHERE json_extract(value, '$.genesis_signature') <> '';"
}

fix_genesis_certificate $AGGREGATOR_NODE $GENESIS_KEY_NEW $GENESIS_VALUE_NEW