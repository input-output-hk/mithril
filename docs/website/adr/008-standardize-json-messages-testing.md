---
slug: 8
title: |
  8. Standardize JSON Message Testing
authors:
  - name: Mithril Team
tags: [Accepted]
date: 2025-01-14
---

## Status

Accepted

## Context

- To ensure backward compatibility and correctness of JSON messages exchanged between nodes, we need a standardized approach
  to test the deserialization of these messages.
- Golden testing is a technique where the expected output (golden data) is stored and used to verify the correctness of
  the system's output. This approach helps in detecting unintended changes in the output and ensures that the system
  behaves as expected over time.
- By using golden testing for JSON message deserialization, we can ensure that any changes to the message structures are
  backward compatible and that the deserialization process yields the expected results.
- We have been using golden testing for JSON messages in the project, but the approach used ad-hoc versions that did not
  correspond to any OpenAPI versions, making it difficult to track the changes and maintain backward compatibility.

## Decision

We will standardize the testing of JSON messages by following the steps below:

When adding a new JSON message structure, the following steps should be taken:

- Introduce a constant `CURRENT_JSON` string containing an exhaustive example of the JSON currently exchanged between nodes.
- Implement a `golden_message_current` method that returns the representation of the `CURRENT_JSON` using the current structure.
- Implement a `test_current_json_deserialized_into_current_message` test that checks that deserializing the `CURRENT_JSON` into the current structure yields the output stored in `golden_message_current`.

When modifying an existing JSON message structure, if backward compatibility is maintained, the following steps should be taken:

- Given `X_Y_ZZ` is the version of the OpenAPI before the change:
  - Create a copy of the previous version structure as it was before the backward-compatible change, suffixed with `UntilVX_Y_ZZ`, e.g., `CertificateMessageUntilV0_1_32`.
  - Create a copy the `golden_message_current` method named `golden_message_until_open_api_X_Y_ZZ`, and update its return type to the version structure suffixed with `UntilVX_Y_ZZ`.
  - Implement a `test_current_json_deserialized_into_message_supported_until_open_api_X_Y_ZZ` test that checks that deserializing the `CURRENT_JSON` into the previous structure yields the output stored in `golden_message_until_open_api_X_Y_ZZ`.
- Modify the `CURRENT_JSON` string to reflect the new structure.
- Modify the `golden_message_current` method to return the representation of the `CURRENT_JSON` using the new structure.

When modifying an existing JSON message structure, if backward compatibility is not maintained, the following steps should be taken:

- Modify the `CURRENT_JSON` string to reflect the new structure.
- Modify the `golden_message_current` method to return the representation of the `CURRENT_JSON` using the new structure.
- Remove all `golden_message_until_open_api_X_Y_ZZ` method and the corresponding structure and tests, as they are no longer relevant.

## Consequences

- Ensures that any changes to the JSON message structure are backward compatible.
- Provides a clear and standardized approach to testing JSON message deserialization.
- Helps maintain the integrity and reliability of the communication between nodes.
- Requires maintaining multiple versions of message structures and corresponding tests, which may increase the maintenance overhead.
