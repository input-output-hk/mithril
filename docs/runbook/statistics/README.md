# Statistics

## Snapshot downloads per snapshot per day

```sh
$> sqlite3 -table -batch \
       $DATA_STORES_DIRECTORY/monitoring.sqlite3 \
       <  snapshot_downloads.sql
```

The variable `$DATA_STORES_DIRECTORY` should point to the directory where the
databases files are stored (see files in `mithril-aggregator/config` using the
key `data_stores_directory` to know where they are).

