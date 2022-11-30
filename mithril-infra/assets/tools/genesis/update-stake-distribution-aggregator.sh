#!/bin/bash
NETWORK=$1
AGGREGATOR_NODE=$2
POOL_ID=$3
POOL_STAKES=$4

exec_sql_aggregator() {
  sqlite3 data/$1/$2/mithril/stores/aggregator.sqlite3 "$3"
}

fix_stake_distribution_aggregator() {
  LAST_EPOCH=$(exec_sql_aggregator $1 $2 "SELECT MAX(key) FROM stake;")

  exec_sql_aggregator $1 $2 "UPDATE stake SET value = json_insert(json(value), '$.${POOL_ID}', ${POOL_STAKES}) WHERE key = '$LAST_EPOCH';"
  exec_sql_aggregator $1 $2 "SELECT * FROM stake WHERE key = $LAST_EPOCH;"
}

fix_stake_distribution_aggregator $NETWORK $AGGREGATOR_NODE $POOL_ID $POOL_STAKES

