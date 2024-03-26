---
title: Mithril signer Prometheus endpoint is available
authors:
  - name: Mithril Team
tags: [mithril signer, metrics, Prometheus, Grafana]
---

### Mithril signer Prometheus endpoint is available

With the release of the new distribution [2412](https://github.com/input-output-hk/mithril/releases/tag/2412.0), the Mithril signer can start an optional Prometheus endpoint to monitor basic metrics.

The endpoint is not activated by default, and needs to be setup by the SPOs who wish to use it. We have added a section in the [Run a Mithril signer as an SPO](https://mithril.network/doc/manual/getting-started/run-signer-node#activate-prometheus-endpoint) guide describing the setup process. 

Please note that the Mithril signer needs to be restarted after the configuration for the Prometheus endpoint.

Additionally, the team is also working on a Grafana template to easily setup a dashboard with this Prometheus endpoint:
[![Grafana Dashboard](img/grafana-dashboard.png)](img/grafana-dashboard.png)

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq). 
