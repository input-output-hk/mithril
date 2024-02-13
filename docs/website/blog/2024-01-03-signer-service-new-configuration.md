---
title: Mithril signer service new configuration
authors:
- name: Mithril Team
tags: [spo, mithril signer, production]
---

### The Mithril signer node service recommended configuration is updated

**PR**: `Fix signer service recommended configuration` [#1419](https://github.com/input-output-hk/mithril/pull/1419)

**Issue**: `Fix signer node service setup` [#1404](https://github.com/input-output-hk/mithril/issues/1404)

The previous recommended configuration proposed a service restart frequency which was too high. When the service was restarted, and if the Cardano node was not ready yet, the service tried to restart too many times in a short period: thus the service just failed and the service had to be started manually after the Cardano node is up and ready. This lead to some SPOs skipping registrations for some epochs.

The duration before restarting the service is increased to 60s: `RestartSec=60`.

Below is the new recommended configuration:

```
[Unit]
Description=Mithril signer service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=cardano
EnvironmentFile=/opt/mithril/mithril-signer.env
ExecStart=/opt/mithril/mithril-signer -vvv

[Install]
WantedBy=multi-user.target
EOF'
```

We highly recommend to update your existing configuration file (`/etc/systemd/system/mithril-signer.service`) with the values specified in the [guide](https://mithril.network/doc/next/manual/getting-started/run-signer-node/#installing-the-service)

:warning: Following this modification, the service needs to be restarted with the following command:

```bash
sudo systemctl restart mithril-signer
```

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
