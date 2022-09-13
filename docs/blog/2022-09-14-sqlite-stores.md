---
title: Mithril internal stores switch to SQLite.
authors:
- name: Mithril Team
tags: [store, sqlite, breaking-change]
---

# Mithril internal stores switch to SQLite.

## What is that?

Since almost the begining of the Mithril project, the software used to rely on a store mechanism to save its different states allowing signers and aggregator to resume on correct state when switched on and off. This internal store mechanism used to be a bunch of JSON files saved in a given directory. Even though this does the job, it still presents flaws: data are hard to query when debugging especially when crossing data (which signers have participated in this multisignature?). Also, data are stored in different places which can be a problem when moving these files from one place to another. We also had to imagine what would be a migration scenario in case of a structure change. Switching to a file based SQL database solves these issues.

The new release now uses SQLite stores in place of JSON file storage. This means that to continue running a signer or an aggregator node it is necessary to migrate from the old storage system to SQLite. This release comes with a tool to perform such migration which should be as straightforward as launching a command line (read below). The migration tool will be available only for a limited time in order to make Mithril beta testers able to migrate their existing data.

## How to migrate data from old storage system to SQLite stores?

The first step is to stop the running Mithril node if any. Then download the new `mithril-signer` and `mithril-signer-migrate` from the [builds page](https://github.com/input-output-hk/mithril/releases).

The `mithril-signer-migrate` executable can perform the migration automatically once you know where your actual JSON files are located. Have a look in your configuration file user the `data_stores_directory` key and copy the path indicated here.

```
$> ./mithril-signer-migrate automatic --db-dir /path/to/json/stores/base/dir
```

At the end of this command, a file `signer.sqlite3` (or `aggregator.sqlite3` if you run an aggrgator) should be present in the specified base directory. 

If you are a Mithril user, that should be enough, launch your upgraded mithril node.

**Note:** The migration executable does not remove the old JSON files from the disk. 

## Manual migration process

The executable also provides a `manual` switch for migrating Mithril JSON store directories placed in custom directories. 

```
$> ./mithril-signer-migrate manual --help
```

The command above should give you all informations needed to run a custom store migration. Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.