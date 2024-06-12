---
title: Mithril client CLI 'snapshot' command is removed
authors:
  - name: Mithril Team
tags: [mithril client, cli, removed, command, snapshot]
---

### The 'snapshot' command of the client CLI is removed

With the release of the new distribution [2423](https://github.com/input-output-hk/mithril/releases/tag/2423.0), we have removed the **snapshot** command of the **client CLI**.

The **snapshot** command has been superseded by the **cardano-db snapshot** command.

The **snapshot** command has been deprecated with the release [2412](https://github.com/input-output-hk/mithril/releases/tag/2412.0), and this has been announced in this [post](https://mithril.network/doc/dev-blog/2024/03/26/client-cli-deprecated-command).

In order to proceed to the upgrade, install the latest stable client CLI, and replace the removed command with the new command, e.g.:
```bash
mithril-client -vvv snapshot download latest --json
```

Should be replaced with
```bash
mithril-client -vvv cardano-db snapshot download latest --json
```

The documentation of the **client CLI** has been updated accordingly and can be found [here](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client#cardano-db).

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq). 
