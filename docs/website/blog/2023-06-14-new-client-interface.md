---
title: Mithril client has got a brand new interface
authors:
- name: Mithril Team
tags: [client, certificate, mithril-stake-distribution]
---

### Mithril client interface is evolving

For the last few months, we have implemented the capability for the Mithril protocol to sign multiple types of data: on top of the already existing Cardano node database snapshots, the Mithril stake distribution is now also signed on its own. 
In order to make the client able to work on the different types of data that are certified, we have changed its command line API.
For example:

```
$> ./mithril-client list
```

This command was previously used to list Cardano node snapshots. It has been abandoned in favor of a more explicit syntax:

```
$> ./mithril-client snapshot list
```

Furthermore, the old version had two different subcommands to 1. download and 2. verify a snapshot. These 2 commands have now be merged into one single `download` command:

```
$> ./mithril-client snapshot download  5109c1eaa6619bc…
```

This organization of the client opens the use of a new `mithril-stake-distribution` sub-command:

```
$> ./mithril-client mithril-stake-distribution list
```

Which can be aliased into a handy

```
$> ./mithril-client msd list
```

As for the Cardano snapshots, it is possible to download and verify the stake distribution involved in Mithril multi-signatures as a JSON file:

```
$> ./mithril-client msd download 713e2803e998f…
```

If the file certification can be verified, it is saved on the disk.

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.