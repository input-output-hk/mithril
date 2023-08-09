---
slug: 5
title: |
  5. Use rfc3339 for date formatting 
authors:
- name: Mithril Team
tags: [Accepted]
date: 2023-06-21
---

## Status

Accepted

## Context

Previously, on the Mithril project we did not have a preferred format for the dates in our applications, leading to
multiple formats being used.

For example when querying a certificate from an aggregator, the `initiated_at` field did not specify the timezone,
timezone that could be found in the `sealed_at` field:
```json
{
  "initiated_at": "2023-05-26T00:02:23",
  "sealed_at": "2023-05-26T00:03:23.998753492Z"
}
```
Same problem in our databases where a date could be stored without timezone and milliseconds (ie: `2023-06-13 16:35:28`)
in one table column and with them in another (ie: `2023-06-13T16:35:28.143292875Z`).

The [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339) is a widely used, easily readable, mostly numeric (no
translation is needed to parse the day or the month), format. Also, it always includes the timezone meaning that our
client can convert such date to their local time if needed.

## Decision

_Therefore_

* We commit to use **RFC 3339** compatible date and time whenever we need to store or show a date and time.

## Consequences

* All dates and time must use a dedicated type in the application, ie: the `DateTime<Utc>` type from
[chrono](https://crates.io/crates/chrono) crate.
  * This means that dates must **never** be stored in our types using Strings.
* Internally, we will always use the **UTC timezone**, to avoid useless conversions between timezones.
* Users or scripts querying dates from our applications or from our databases will be able to parse all of them using
the same format.