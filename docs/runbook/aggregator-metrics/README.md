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

Another view `metrics_per_day_and_origin` is available to compute the aggregated values of metrics by day with the origin tag.

The following request displays the aggregated value of each metric by day with the origin tag:

```sh
sqlite3 -table -batch \
       <DATA_STORES_DIRECTORY>/monitoring.sqlite3 \
       `select date, counter_name, origin, value from metrics_per_day_and_origin order by date desc, counter_name asc;`
```

The result looks like:

```
+------------+------------------------------------------------------------------------------------------+--------+-----------+
|    date    |                                       counter_name                                       | value  |  origin   |
+------------+------------------------------------------------------------------------------------------+--------+-----------+
| 2025-04-10 | certificate_detail_total_served_since_startup                                            | 8636   | CI        |
| 2025-04-10 | certificate_detail_total_served_since_startup                                            | 152    | EXPLORER  |
| 2025-04-10 | certificate_detail_total_served_since_startup                                            | 1873   | NA        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup           | 20     | CI        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup           | 3      | NA        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 12     | CI        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 4      | NA        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup | 24     | CI        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup | 4      | NA        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup        | 12     | CI        |
| 2025-04-10 | mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup        | 2      | NA        |
| 2025-04-10 | mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup | 12     | CI        |
| 2025-04-10 | mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup | 2      | NA        |
| 2025-04-10 | mithril_aggregator_cardano_db_complete_restoration_since_startup                         | 8      | CI        |
| 2025-04-10 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 8      | CI        |
| 2025-04-10 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 3      | NA        |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 6000   | BENCHMARK |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 12     | CI        |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 1      | EXPLORER  |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 6003   | NA        |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 151000 | BENCHMARK |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 24     | CI        |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 1      | EXPLORER  |
| 2025-04-10 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 151007 | NA        |
| 2025-04-10 | mithril_aggregator_signature_registration_total_received_since_startup                   | 303    | NA        |
| 2025-04-10 | mithril_aggregator_signer_registration_total_received_since_startup                      | 3      | NA        |
| 2025-04-09 | certificate_detail_total_served_since_startup                                            | 4288   | CI        |
| 2025-04-09 | certificate_detail_total_served_since_startup                                            | 1572   | NA        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup           | 10     | CI        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup           | 5      | NA        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 6      | CI        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup                 | 4      | NA        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup | 12     | CI        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup | 4      | NA        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup        | 6      | CI        |
| 2025-04-09 | mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup        | 2      | NA        |
| 2025-04-09 | mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup | 6      | CI        |
| 2025-04-09 | mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup | 2      | NA        |
| 2025-04-09 | mithril_aggregator_cardano_db_complete_restoration_since_startup                         | 4      | CI        |
| 2025-04-09 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 4      | CI        |
| 2025-04-09 | mithril_aggregator_cardano_db_total_restoration_since_startup                            | 3      | NA        |
| 2025-04-09 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 6      | CI        |
| 2025-04-09 | mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup           | 2      | NA        |
| 2025-04-09 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 18     | CI        |
| 2025-04-09 | mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup     | 6      | NA        |
| 2025-04-09 | mithril_aggregator_signature_registration_total_received_since_startup                   | 664    | NA        |
| 2025-04-09 | mithril_aggregator_signer_registration_total_received_since_startup                      | 6      | NA        |
| 2025-04-08 | certificate_detail_total_served_since_startup                                            | 145    | NA        |
| 2025-04-08 | mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup           | 7      | NA        |
| 2025-04-08 | mithril_aggregator_signature_registration_total_received_since_startup                   | 306    | NA        |
| 2025-04-08 | mithril_aggregator_signer_registration_total_received_since_startup                      | 3      | NA        |
+------------+------------------------------------------------------------------------------------------+--------+-----------+
```

The variable `<DATA_STORES_DIRECTORY>` should point to the directory where the
databases files are stored (see files in `mithril-aggregator/config` using the
key `data_stores_directory` to know where they are).
