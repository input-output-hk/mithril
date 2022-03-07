# Running the Rust node

A simple config file for a 2 node configuration is in `config/local_testing.json`

To run node 1:

```
cargo run -- --config-file config/local_testing.json --node-id 1
```

To run node 2:

```
cargo run -- --config-file config/local_testing.json --node-id 2
```

The `--node-id` argument refers to the `"id"` field of the configuration file.