# Monitoring

## Stakes vs Signer Version distribution

In order to set the epoch of Era changes, it is required to know the
distribution of the stakes that run a compatible Signer node. There is a SQL
query for that.

```sh
$> sqlite3 -table -batch \
       $DATA_STORES_DIRECTORY/monitoring.sqlite3 \
       <  stake_signer_version.sql
```

The variable `$DATA_STORES_DIRECTORY` should point to the directory where the
databases files are stored (see files in `mithril-aggregator/config` using the
key `data_stores_directory` to know where they are).
