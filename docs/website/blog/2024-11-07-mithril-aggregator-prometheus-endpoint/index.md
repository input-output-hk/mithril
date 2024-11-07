---
title: Mithril aggregator Prometheus endpoint is available
authors:
  - name: Mithril Team
tags: [mithril aggregator, metrics, Prometheus, Grafana]
---

### Mithril aggregator Prometheus endpoint is available

With the release of the new distribution [2445](https://github.com/input-output-hk/mithril/releases/tag/2445.0), the Mithril aggregator can start an optional Prometheus endpoint to monitor basic metrics.

Please note that the Mithril aggregator needs to be restarted after the configuration for the Prometheus endpoint.

Additionally, a **Grafana template** has been created to easily setup a dashboard for this Prometheus endpoint (ID 22165): https://grafana.com/grafana/dashboards/22165-mithril-aggregator/

[![Grafana Dashboard](img/grafana-dashboard.png)](img/grafana-dashboard.png)

For any inquiries or assistance, don't hesitate to reach out to the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
