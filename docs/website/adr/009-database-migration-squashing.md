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

Many database migrations have accumulated over time for the Mithril nodes. As the migrations are done sequentially, the resulting scheme of the database is hard to understand and maintain.

## Decision

To mitigate these concerns, we have decided to implement a migration squashing once we have stacked too many migrations for a store: all the existing migrations are consolidated and replaced by the equivalent new migration.

## Consequences

- This applies to the migrations of all the stores of the Mithril nodes
- A squashed migration will be applied when a database is initialized for the first time
- A squashed migration must be optional and run only if not already applied previously with the equivalent migration sequence
- Some nodes may have only partially applied the equivalent sequence of migrations and thus can not apply the squashed migration right away:
  - They need to first run the migration with the latest distribution that does not ship the squashed migration so that their database is ready to play the squashed migration
  - This distribution is associated to a squashed migration to provide a smooth user experience.
