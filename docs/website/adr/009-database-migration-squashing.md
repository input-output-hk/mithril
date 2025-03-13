---
slug: 9
title: |
  9. Database migration squashing
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2025-03-13
---

## Status

Accepted

## Context

Over time, many database migrations have accumulated in Mithril nodes. Since these migrations are applied sequentially, the resulting database schema has become difficult to understand and maintain.

## Decision

To address this, the team decided to implement migration squashing once too many migrations have accumulated for a store. This process consolidates all existing migrations into a single, equivalent migration.

## Consequences

- This applies to the migrations of all Mithril node stores
- A squashed migration will be applied when a database is initialized for the first time
- A squashed migration must be optional and should only run if it has not been previously applied with the equivalent migration sequence
- Some nodes may have only partially applied the equivalent sequence of migrations and cannot apply the squashed migration immediately:
  - They must first run the migration using the latest distribution that does not include the squashed migration, ensuring their database is prepared to apply it
  - This distribution is associated with a squashed migration to provide a smooth user experience.
