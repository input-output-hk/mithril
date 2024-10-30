# Aggregator metrics

A view `metrics_per_day` is available to calculate the value of a metric over a day.

The following request displays the sum of counter for each metric on the specify day.
Format of `DAY` variable should be `YYYY-MM-DD` (ie: `2024-10-28`).

```sh
$> sqlite3 -table -batch \
       $DATA_STORES_DIRECTORY/monitoring.sqlite3 \
       `select date, counter_name, value from metrics_per_day where date='$DAY';`
```

The result looks like:

```
+------------+-------------------------------------------------------------+--------+
|    date    |                        counter_name                         | value  |
+------------+-------------------------------------------------------------+--------+
| 2024-10-29 | mithril_aggregator_certificate_total_produced_since_startup | 135532 |
| 2024-10-29 | mithril_aggregator_runtime_cycle_success_since_startup      | 563246 |
| 2024-10-29 | mithril_aggregator_runtime_cycle_total_since_startup        | 237513 |
+------------+-------------------------------------------------------------+--------+
```
