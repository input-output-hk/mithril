---
title: Mithril internal stores switch to SQLite.
authors:
- name: Mithril Team
tags: [store, sqlite, breaking-change]
---

## What is that?

Since almost the beginning of the Mithril project, the software used to rely on a store mechanism to save its different states allowing Signers and Aggregators to resume on correct state when switched on and off. This internal store mechanism used to be a bunch of JSON files saved in a given directory. Even though this does the job it still presents flaws: data are hard to query when debugging especially when crossing data (which signers have participated in this multi-signature?). Also, data are stored in different places which can be a problem when moving these files from one place to another. We also had to imagine what would be a migration scenario in case of a structure change. Switching to a file based SQL database solves these issues.

The new release now uses SQLite stores in place of JSON file storage. This means that to continue running a Signer or an Aggregator node it is necessary to migrate from the old storage system to SQLite. This release comes with a tool to perform the migration which should be as straightforward as launching a command line (read below). The migration tool will be available only for a limited time in order to make Mithril beta testers able to migrate their existing data.

## How to migrate data from old storage system to SQLite stores?

There are 2 ways of getting the new version and the associated migration tool. Either downloading binaries from GitHub or compiling them yourself.

### Downloading

Download the new `mithril-signer` and `mithril-signer-migrate` files from the [nightly builds page](https://github.com/input-output-hk/mithril/releases/tag/nightly). Make them executable:

```
$> chmod +x mithril-signer*
$> ls -1F mithril-signer*
mithril-signer*
mithril-signer-migrate*
```

_note_: the suffix `*` appended to the the entries output above indicates the file is executable. If it is not present, ensure the `chmod` command does not produce any error.

### Compiling

If you used to compile your node as stated in the [guide](https://mithril.network/doc/manual/getting-started/run-signer-node), you have to compile the migration tool as well:

```
$> cd mithril-signer
$> cargo build --all-targets --release
  Compiling mithril-signer v0.1.0 (/home/somebody/shared/mithril/mithril-signer)
    Finished release [optimized] target(s) in 4.56s
$> ls -1F ../target/release/mithril-signer*
../target/release/mithril-signer*
../target/release/mithril-signer.d
../target/release/mithril-signer-migrate*
../target/release/mithril-signer-migrate.d
```

### Running the migration

The first step is to stop the running Mithril node if any. The `mithril-signer-migrate` executable can perform the migration automatically once you know where your actual JSON files are located. Have a look in your configuration file (default `/opt/mithril/mithril-signer/service.env`), check the value associated with the `DATA_STORES_DIRECTORY` key (default to `/opt/mithril/mithril-signer/stores`) and copy the path indicated here. Copy this path after the `--db-dir` option on the following command line:

```
$> ./mithril-signer-migrate automatic --db-dir /paste/the/data/stores/directory/here
Mithril Aggregator JSON → SQLite migration tool.
Migrating protocol_initializer_store data…
OK ✓
Migrating stake_store data…
OK ✓
```

At the end of this command, a file `signer.sqlite3` (or `aggregator.sqlite3` if you run an Aggregator) should be present in the specified base directory. 

That should be enough, launch your upgraded mithril node.

**Note:** The migration executable does not remove the old JSON files from the disk. 

### Manual migration process

The executable also provides a `manual` switch for migrating Mithril JSON store directories placed in custom directories. This is mainly intended for developers who work on tweaked environments. Each internal store has its own data structure. In order to correctly migrate and process data, the type of the store has to be given on the command line.

```
$> ./mithril-signer-migrate manual --help
```

The command above should give you all informations needed to run a custom store migration. 

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.