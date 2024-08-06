---
title: Mithril client CLI 'snapshot' command is deprecated
authors:
  - name: Mithril Team
tags: [mithril client, cli, deprecated]
---

### The 'snapshot' command of the client CLI is deprecated

With the release of the new distribution [2412](https://github.com/input-output-hk/mithril/releases/tag/2412.0), we have deprecated the **snapshot** command of the client CLI.

The **snapshot** command has been superseded by the **cardano-db snapshot** command.
In order to give time to our users to use the new command, the now legacy **snapshot** command is still available, but it will be removed in a near future.

In order to proceed to the upgrade, install the latest stable client CLI, and replace the legacy command with the new command, e.g.:

```bash
mithril-client -vvv snapshot download latest --json
```

Should be replaced with

```bash
mithril-client -vvv cardano-db snapshot download latest --json
```

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
