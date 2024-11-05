# Monitoring

## Stakes vs Signer Version distribution

In order to set the epoch of Era changes, it is required to know the
distribution of the stakes that run a compatible Signer node. There is a SQL
view for that.

```sh
$> sqlite3 -table -batch \
       $DATA_STORES_DIRECTORY/monitoring.sqlite3 \
        `select epoch, version, total_epoch_stakes, stakes_version, stakes_ratio, pool_count from signer_registration_summary;`
```

The variable `$DATA_STORES_DIRECTORY` should point to the directory where the
databases files are stored (see files in `mithril-aggregator/config` using the
key `data_stores_directory` to know where they are).
