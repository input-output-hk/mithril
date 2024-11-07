# Aggregator metrics

A view `metrics_per_day` is available to compute the aggregated values of metrics by day.

The following request displays the aggregated value of each metric by day:

```sh
sqlite3 -table -batch \
       <DATA_STORES_DIRECTORY>/monitoring.sqlite3 \
       `select date, counter_name, value from metrics_per_day order by date desc, counter_name asc;`
```

The result looks like:

```
+------------+------------------------------------------------------------------------------------------+-------+
|    date    |                                       counter_name                                       | value |
+------------+------------------------------------------------------------------------------------------+-------+
| 2024-11-07 | mithril_aggregator_artifact_cardano_db_total_produced_since_startup                      | 14    |
| 2024-11-07 | mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup      | 1     |
| 2024-11-07 | mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup             | 143   |
| 2024-11-07 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 5     |
| 2024-11-07 | mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup      | 1     |
| 2024-11-07 | mithril_aggregator_certificate_detail_total_served_since_startup                         | 3076  |
| 2024-11-07 | mithril_aggregator_certificate_total_produced_since_startup                              | 159   |
| 2024-11-07 | mithril_aggregator_runtime_cycle_success_since_startup                                   | 899   |
| 2024-11-07 | mithril_aggregator_runtime_cycle_total_since_startup                                     | 900   |
| 2024-11-07 | mithril_aggregator_signature_registration_total_received_since_startup                   | 1054  |
| 2024-11-07 | mithril_aggregator_signer_registration_total_received_since_startup                      | 19    |
| 2024-11-06 | mithril_aggregator_artifact_cardano_db_total_produced_since_startup                      | 21    |
| 2024-11-06 | mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup      | 1     |
| 2024-11-06 | mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup             | 213   |
| 2024-11-06 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 8     |
| 2024-11-06 | mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup      | 1     |
| 2024-11-06 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 3     |
| 2024-11-06 | mithril_aggregator_certificate_detail_total_served_since_startup                         | 4326  |
| 2024-11-06 | mithril_aggregator_certificate_total_produced_since_startup                              | 236   |
| 2024-11-06 | mithril_aggregator_runtime_cycle_success_since_startup                                   | 1426  |
| 2024-11-06 | mithril_aggregator_runtime_cycle_total_since_startup                                     | 1426  |
| 2024-11-06 | mithril_aggregator_signature_registration_total_received_since_startup                   | 2204  |
| 2024-11-06 | mithril_aggregator_signer_registration_total_received_since_startup                      | 26    |
| 2024-11-05 | mithril_aggregator_artifact_cardano_db_total_produced_since_startup                      | 7     |
| 2024-11-05 | mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup             | 85    |
| 2024-11-05 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 11    |
| 2024-11-05 | mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup | 14    |
| 2024-11-05 | mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup        | 7     |
| 2024-11-05 | mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup | 7     |
| 2024-11-05 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 7     |
| 2024-11-05 | mithril_aggregator_certificate_detail_total_served_since_startup                         | 20306 |
| 2024-11-05 | mithril_aggregator_certificate_total_produced_since_startup                              | 92    |
| 2024-11-05 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 3008  |
| 2024-11-05 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 75508 |
| 2024-11-05 | mithril_aggregator_runtime_cycle_success_since_startup                                   | 513   |
| 2024-11-05 | mithril_aggregator_runtime_cycle_total_since_startup                                     | 513   |
| 2024-11-05 | mithril_aggregator_signature_registration_total_received_since_startup                   | 1145  |
| 2024-11-05 | mithril_aggregator_signer_registration_total_received_since_startup                      | 3     |
+------------+------------------------------------------------------------------------------------------+-------+
```

The variable `<DATA_STORES_DIRECTORY>` should point to the directory where the
databases files are stored (see files in `mithril-aggregator/config` using the
key `data_stores_directory` to know where they are).
