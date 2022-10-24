#!/bin/bash
AGGREGATOR_NODE=$1

exec_sql_aggregator() {
  sqlite3 data/preview/$1/mithril/stores/aggregator.sqlite3 "$2"
}

fix_table_aggregator() {
  LAST_EPOCH=$(exec_sql_aggregator $1 "SELECT MAX(key) FROM $2;")
  LAST_VALUE=$(exec_sql_aggregator $1 "SELECT value FROM $2 WHERE key = $LAST_EPOCH;")

  CREATE_EPOCH=$LAST_EPOCH
  CREATE_EPOCH=$(expr $CREATE_EPOCH - $3)
  CREATE_EPOCH_HASH=$(echo -n $CREATE_EPOCH | sha256sum | awk '{print $1}')
  exec_sql_aggregator $1 "INSERT OR IGNORE INTO $2(key_hash, key, value) VALUES('$CREATE_EPOCH_HASH','$CREATE_EPOCH','$LAST_VALUE');"
  exec_sql_aggregator $1 "SELECT * FROM $2 WHERE key = $CREATE_EPOCH;"
}

fix_table_aggregator $AGGREGATOR_NODE stake 1
fix_table_aggregator $AGGREGATOR_NODE stake 2
fix_table_aggregator $AGGREGATOR_NODE stake 3
fix_table_aggregator $AGGREGATOR_NODE verification_key 1
fix_table_aggregator $AGGREGATOR_NODE verification_key 2
fix_table_aggregator $AGGREGATOR_NODE verification_key 3
fix_table_aggregator $AGGREGATOR_NODE protocol_parameters 1
fix_table_aggregator $AGGREGATOR_NODE protocol_parameters 2
fix_table_aggregator $AGGREGATOR_NODE protocol_parameters 3

