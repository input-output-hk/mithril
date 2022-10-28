#!/bin/bash
NETWORK=$1
AGGREGATOR_NODE=$2

exec_sql_aggregator() {
  sqlite3 data/$1/$2/mithril/stores/aggregator.sqlite3 "$3"
}

fix_table_aggregator() {
  LAST_EPOCH=$(exec_sql_aggregator $1 $2 "SELECT MAX(key) FROM $3;")
  LAST_VALUE=$(exec_sql_aggregator $1 $2 "SELECT value FROM $3 WHERE key = $LAST_EPOCH;")

  CREATE_EPOCH=$LAST_EPOCH
  CREATE_EPOCH=$(expr $CREATE_EPOCH - $4)
  CREATE_EPOCH_HASH=$(echo -n $CREATE_EPOCH | sha256sum | awk '{print $1}')
  exec_sql_aggregator $1 $2 "INSERT OR IGNORE INTO $3(key_hash, key, value) VALUES('$CREATE_EPOCH_HASH','$CREATE_EPOCH','$LAST_VALUE');"
  exec_sql_aggregator $1 $2 "SELECT * FROM $3 WHERE key = $CREATE_EPOCH;"
}

fix_table_aggregator $NETWORK $AGGREGATOR_NODE stake 1
fix_table_aggregator $NETWORK $AGGREGATOR_NODE stake 2
fix_table_aggregator $NETWORK $AGGREGATOR_NODE stake 3
fix_table_aggregator $NETWORK $AGGREGATOR_NODE verification_key 1
fix_table_aggregator $NETWORK $AGGREGATOR_NODE verification_key 2
fix_table_aggregator $NETWORK $AGGREGATOR_NODE verification_key 3
fix_table_aggregator $NETWORK $AGGREGATOR_NODE protocol_parameters 1
fix_table_aggregator $NETWORK $AGGREGATOR_NODE protocol_parameters 2
fix_table_aggregator $NETWORK $AGGREGATOR_NODE protocol_parameters 3

