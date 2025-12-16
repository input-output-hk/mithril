# Mithril Common Architecture Decision Records

---

<!--
Template of ADR

## ID. TITLE

date: 2025-XX-XX
status: Accepted

### Context

To complete

### Decision

To complete

### Consequences

To complete
-->

### 5. Guidelines for writing useful log messages, error context, or error structure

**Date:** 2025-07-25
**Status:** Accepted

### Context

Some errors and logs currently lack enough context to understand the cause of an issue.

This is especially true for failures that occur during requests to or from external sources.

At the same time, adding too much context can make logs noisy and hard to read, and can flood log storage with low-value
or sensitive data.

### Decision

When writing log messages, adding error context, or designing error structures, follow these guidelines:

- **Prefer structured logging for internal context.**
  - Add identifiers as structured fields rather than embedding them in the message text (e.g., party id, signed entity
    type, beacon, request id, entity id).
  - Keep the human-readable message short, put “what happened” in the message and “what it relates to” in structured fields.

- **Avoid unnecessary or sensitive context in logs by default.**
  - Do not log secrets or high-risk material (e.g., cryptographic keys, seeds, tokens, credentials).
  - Do not log large payloads unless they are required for troubleshooting (see “External sources” below).

- **Handle large debug output explicitly.**
  - If a type’s `Debug` output is too large or contains sensitive fields, implement `Debug` manually to provide a safe,
    non-exhaustive representation by default.
  - Optionally support an “alternate” representation (e.g., `{:#?}`) that includes additional detail when it is safe and useful.

- **External sources: allow exceptions when needed to troubleshoot.**
  - For interactions with external sources, it can be acceptable to include additional context such as request/response
    payloads **only when necessary** to diagnose issues.
  - When logging external payloads, prefer safeguards such as truncation/size limits and logging only at error/debug
    level (and redaction when applicable).

### Consequences

- Logs are more readable and actionable.
- Errors are easier to understand and troubleshoot without routinely leaking sensitive data or producing excessive log volume.

### 4. Guidelines for crate test utilities

**Date:** 2025-07-25
**Status:** Accepted

### Context

- Testing requires reusable utilities that may need to be shared across crates
- Test utilities should be isolated from production code while remaining accessible to child crates
- We need to minimize feature flags to optimize Rust compiler artifact reuse and reduce build times

### Decision

Test utilities must follow this organizational structure:

**Core Rules:**

1. All test utilities belong in a dedicated `test` module within each crate
2. Utilities become public only when used by child crates or integration tests
3. Public test utilities must not introduce additional dependencies
4. Private test utilities are gated behind `cfg(test)`
5. Import paths must explicitly include `test` modules to prevent accidental production usage
6. Feature flags are prohibited for test utility isolation

**Module Organization:**

- **Test doubles** (mocks, fakes, stubs): `test::double` module
- **Test data builders**: `test::builder` module
- **Test-only type extensions**: Extension traits in `test` module
  - Trait names end with `TestExtension`
  - Implementations follow trait definitions, except when accessing private fields

#### Consequences

- Consistent codebase organization across all crates
- Clear separation between production and test code
- Improved discoverability and maintainability of test utilities
- Reduced build times through minimal feature flag usage
- Enhanced reusability of test utilities across child crates

---

### 3. Guidelines for Dummy Test Doubles

**Date:** 2025-07-22
**Status:** Accepted

#### Context

The use of `dummy()` functions for creating test doubles is widespread across the codebase. However, inconsistencies
in their placement and visibility have led to maintenance challenges and reduced code clarity.

#### Decision

A `Dummy` trait will be introduced, functioning similarly to Rust's `Default` trait.

The following guidelines will be adopted for implementing the `Dummy` trait:

1. Most implementations should reside in a `test::double::dummies` module within the crate where the type is defined.
2. For types with non-public fields, the `Dummy` trait should be implemented directly below the type's definition.

#### Consequences

- Enhanced consistency in code organization.
- Improved discoverability of test doubles.
- Clearer distinction between production and test code.
- Simplified maintenance of test implementations.

---

## 2. Remove artifacts serialization support when compiling to WebAssembly

date: 2025-02-26
status: Accepted

### Context

After the update to rust `1.85` on `2025-02-21`, we noticed that `mithril-client-wasm` was failing to compile with the error:

```
[INFO]: ⬇️  Installing wasm-bindgen...
thread 'main' panicked at crates/wasm-interpreter/src/lib.rs:245:21:
mithril_common::signable_builder::interface::_::__ctor::h4977fb9f7c35308c: Read a negative address value from the stack. Did we run out of memory?
```

Investigating the problem, we found that the use of `typetag::serde` attribute was causing the issue, as removing them
allowed the project to compile successfully.
Furthermore, we found that this is a known issue with `typetag` and WebAssembly, as documented in the `typetag` repository
[issue #54](https://github.com/dtolnay/typetag/issues/54).
Why this issue wasn't happening before the update to rust `1.85` is still unknown, as this incompatibility predates the update.

We use the `typetag::serde` attribute to serialize and deserialize Artifacts which are implementing a common trait.
As of today, this serialization is only used by `mithril-aggregator` to store the Artifacts in the database.

### Decision

We will remove the `typetag::serde` attribute from the Artifacts when compiling to WebAssembly.

### Consequences

Web Assembly will not be able to serialize and deserialize Artifacts using the generic `Artifact` trait.
This will not affect `mithril-aggregator` as it is not compiled to WebAssembly.

In the future, if we need to serialize and deserialize Artifacts in WebAssembly, we will need to find an alternative solution.

---

## 1. Record architecture decisions

date: 2025-02-26
status: Accepted

### Context

We already have a few ADRs in the `docs/website/adr` directory which document project-wide architectural decisions.
But we also want to document the rationale behind smaller decisions, so they are not lost in the shuffle, avoiding
the need to rehash the same discussions in the future.

### Decision

We will use Architecture Decision Records, as described by Michael Nygard in this article: http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions

To keep things simple, we will store these ADRs in a single file, and use a simple format to keep them readable.

### Consequences

See Michael Nygard's article, linked above.
