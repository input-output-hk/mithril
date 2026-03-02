---
slug: 11
title: |
  11. Infrastructure-only redeployment without new distribution
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2026-03-02
---

## Status

Accepted

## Context

The Mithril release process (see [ADR 3](/adr/3)) is designed around distributing new versions of the Mithril software. Every release cycle builds new artifacts, creates Docker images, publishes crates and npm packages, and deploys the infrastructure.

However, some changes do not require a new Mithril distribution at all. A typical example is a Cardano node upgrade that is fully compatible with the current Mithril nodes: the Mithril binaries and Docker images remain unchanged, but the infrastructure must be redeployed with updated configuration (e.g. new Cardano node version, new configuration files).

Previously, this required going through the full release cycle, creating a new distribution tag even though the Mithril software itself introduced no new features. This was inefficient and added unnecessary overhead to the release process.

## Decision

The `pre-release` and `release` GitHub workflows now support manual triggering (`workflow_dispatch`) to redeploy the infrastructure without creating a new distribution.

When triggered manually:

- Only the deployment jobs (`deploy-pre-release` / `deploy-release`) run.
- All other jobs (Docker build, GitHub release creation, crate/npm publishing) are skipped.
- The caller provides the `mithril_image_id` of the existing Docker image to deploy.
- A `dry_run` mode (enabled by default) allows verifying the terraform plan before applying it.

The process to perform an infrastructure-only redeployment is:

1. Create a branch with the infrastructure modifications since the last release (e.g. updated Cardano node configuration files in `mithril-infra/`).
2. Run the **Pre-release** workflow manually from that branch:
   - Provide the `mithril_image_id` of the current distribution already deployed.
   - Enable `dry_run` to verify the terraform plan.
   - Disable `dry_run` to apply the deployment to `pre-release-preview`.
3. Once validated, run the **Release** workflow manually from that branch:
   - Provide the same `mithril_image_id`.
   - Enable `dry_run` to verify the terraform plan.
   - Disable `dry_run` to apply the deployment to `release-preprod` and `release-mainnet`.
4. Merge the branch back to `main`.

## Consequences

- Cardano node upgrades that are compatible with the current Mithril distribution no longer require a new release tag or distribution version.
- The existing tag-push and release-event triggered behavior of both workflows is completely unchanged.
- The `dry_run` mode provides a safety net by allowing operators to review the terraform plan before any actual deployment.
- The `mithril_image_id` input must reference an existing Docker image in the registry; it is the caller's responsibility to provide a valid value.
- Infrastructure changes must still be merged back to `main` after deployment to keep the repository in sync with the deployed state.
