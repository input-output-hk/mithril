# Aggregator metrics

Aggregator metrics are stored in `monitoring.sqlite3` database as an event.

They are stored in database with `Metrics` as `source` and the name of the metric as `action`.

| event_id | created_at                          | source  | action                                                              | content                                                                                                           |
| -------- | ----------------------------------- | ------- | ------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| 104      | 2024-10-29T10:27:49.843362986+00:00 | Metrics | mithril_aggregator_signer_registration_total_received_since_startup | `{"content":{"counter":2,"date":"2024-10-29T10:27:49.842316726Z","duration":{"nanos":0,"secs":3}},"headers":{}} ` |
|          |

The content of the metric is a json that contains the counter value, the date the value was measured and the duration that corresponds to the measurement.

Json sample:

```json
{
  "content": {
    "counter": 6,
    "date": "2024-10-28T16:43:18.858027008Z",
    "duration": {
      "nanos": 0,
      "secs": 10
    }
  },
  "headers": {}
}
```

Each event represents the number of hits on the counter since the last period of timed.

A view `metrics_per_day` is available to calculate the value of a metric over a day.

The following request displays the sum of counter for each metric on the specify day.
`select date, counter_name, value from metrics_per_day where date='2024-10-29';`

The result looks like:

```
2024-10-29|mithril_aggregator_certificate_total_produced_since_startup|32
2024-10-29|mithril_aggregator_runtime_cycle_success_since_startup|131
2024-10-29|mithril_aggregator_runtime_cycle_total_since_startup|239
```

NOTE: These metrics should not be used to have an exact count. There may be slight discrepancies with reality.
When stopping the aggregator, last measure may not be counted.

The measurement extends over a period of time which may have started the previous day.
In this case, the value is counted for the day the measurement was taken.

The measurement frequency is relatively short and can be configured by setting the environment variable `PERSIST_USAGE_REPORT_INTERVAL_IN_SECONDS`.
