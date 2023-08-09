---
title: Genesis Certificate support added
authors:
- name: Mithril Team
tags: [genesis, certificate, breaking-change]
---

**Update**: The PR has been merged and the feature is being deployed on the GCP Mithril Aggregator.

### This afternoon, we plan to merge the PR that activates the Genesis Certificate feature on the GCP Mithril Aggregator

**PR**: `Implement Real Genesis Certificate` [#438](https://github.com/input-output-hk/mithril/pull/438)

**Issue**: `Bootstrap Certificate Chain w/ Genesis Certificate` [#364](https://github.com/input-output-hk/mithril/issues/364)

This will involve some manual operations that will prevent temporarily the service to be running:

* We will have to reset the stores of the `Snapshots` and `Certificates`. This means that the [Mithril Explorer](https://mithril.network/explorer/) will display a `No snapshot available` message.

* The Mithril Signers will have to wait until the next epoch `#30` to be able to sign. This means that we should see the first available `Snapshot` 1 hour after the epoch transition.

The SPOs that are currently running a Mithril Signer will have to recompile their node in order ot take advantage of the latest improvements (such as the registration of the nodes that will take few minutes instead of few hours). However, the previously compiled node will be able to contribute to signatures.

In order to restore a Mithril Snapshot, a Mithril Client will now need access to the `Genesis Verification Key` by adding an environment variable when running: `GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey)`.

Feel free to reach out to us on the [Discord channel](https://discord.gg/5kaErDKDRq) for questions and/or help.
