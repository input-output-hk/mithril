---
slug: 7
title: |
  7. Standardize log output
authors:
- name: Mithril Team
tags: [Accepted]
date: 2024-04-07
---

## Status

Accepted

## Context

* [ADR 2](/adr/2) is not completely relevant now, we have migrated recently the logs in the client to `stderr`. Only the result of the command execution is in `stdout`. This makes it possible to exploit the result, see our [blog post](/dev-blog/2024/02/26/mithril-client-cli-output-breaking-change).
* Mithril aggregator logs are always redirected to `stdout` but it mixes 2 types of CLI commands, some of which would benefit from the logs output to `stderr`.
* Mithril aggregator and Mithril client CLI have not a consistent log strategy, that's why we need to standardize them.

## Decision

* For commands that provide a result or execute an action, logs are sent to `stderr`. Only the result of the command is sent to `stdout`.
* For commands that launch a program without an expected result (server), logs are sent to `stdout`.

## Consequences

* End users who use `stdout` logs would have a breaking change. They will have to retrieve the logs that come from `stderr` in addition.
* Commands `genesis`, `era` and `tools` from Mithril aggregator now send their logs to `stderr`.
