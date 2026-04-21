---
title: Mithril signer `1.0.0` stable release
authors:
  - name: Mithril Team
tags: [release, distribution, 2617, signer, stable, production]
---

### Mithril signer `1.0.0` is production-ready

With the release of the [`2617.0`](https://github.com/input-output-hk/mithril/releases/tag/2617.0) distribution, the **Mithril signer** has reached its **first stable version** `1.0.0` and is now officially **production-ready** on the `release-mainnet` network.

#### What does this mean?

Since the [Mainnet Beta launch](https://mithril.network/doc/dev-blog/2023/07/21/mainnet-beta-launch) in July 2023, the Mithril signer has been continuously improved, hardened, and validated by the community of stake pool operators (SPOs) running it on the Cardano `release-mainnet` network. The `1.0.0` release marks the moment where the team considers the signer:

- **Stable** in terms of public API, configuration, and operational behavior
- **Performant** across supported platforms (Linux x86_64, Linux ARM64)
- **Secure** thanks to many iterations of review and hardening of the signing pipeline
- **Reliable** for long-running production deployments on `release-mainnet`.

From this release onwards, backward-incompatible changes to the Mithril signer follow the [semantic versioning](https://semver.org/spec/v2.0.0.html) convention and will be announced in dedicated dev blog posts.

#### Upgrade path

If you operate a Mithril signer on `release-mainnet` or `release-preprod`, upgrade to version `1.0.0` as part of the distribution [`2617.0`](https://github.com/input-output-hk/mithril/releases/tag/2617.0) rollout (no configuration changes are required):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d 2617.0 -p $(pwd)
```

:::info

A huge thank you to all the pioneer SPOs who have been running the Mithril signer and providing invaluable feedback since the very first iterations. This stable release would not have been possible without you.

:::

For any inquiries or assistance, contact the team on the [Discord channel](https://discord.gg/5kaErDKDRq).
