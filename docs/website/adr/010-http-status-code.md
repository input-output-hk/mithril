---
slug: 10
title: |
  10. Specific Mithril Http status code
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2025-03-21
---

## Status

Accepted

## Context

In exchanges between the signer and the aggregator, we need to retrieve the reason why a request was unsuccessful.
Error handling will depend on the specific functional case of Mithril.
We could have reused existing HTTP codes, but they are too general and could be returned for cases other than the one we wish to isolate.

## Decision

We therefore decided to return specific error codes when we need to identify the functional case.
We start at 450 for client error codes and at 550 for server error codes.

## Consequences

Specific Mithril HTTP status code on server error should be between 550 and 599.
For client error the HTTP status code should be between 450 and 499.
