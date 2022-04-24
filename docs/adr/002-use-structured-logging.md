---
slug: 2
title: |
  2. Use simple structured logging
authors: []
tags: [Draft]
---

## Status

**Draft**

## Context

* Logs are a critical tool for operating any software system, enabling [observability](https://cloud.google.com/architecture/devops/devops-measurement-monitoring-and-observability) of the system.
* Following [12 Factor Apps](https://12factor.net/logs) principles, providing the needed components and tools to be able to configure logging and monitoring should not be the responsibility of the software components

## Decision

_Therefore_

* Each component of the system use [Structured logging](https://www.sumologic.com/glossary/structured-logging/) using documented and standardised JSON format for its logs
* Logs are always emitted to `stdout` of the process the component is part of

## Consequences

* The schema of the logged items should be properly documented in a JSON schema
* It is the responsibility of the node operator to consume the logs and process them
* We use existing libraries to provide needed log infrastructure, like [slog](https://zsiciarz.github.io/24daysofrust/book/vol2/day4.html) for Rust
