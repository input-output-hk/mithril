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

### 4. Guidelines for crate test utilities

date: 2025-07-25
status: Accepted

### Context

- For testing, crates may need to define reusable test utilities
- Some of those utilities may need to be exposed to child crates to enable or facilitate writing tests
- We want to isolate test utilities from production code as much as possible
- We want to limit the number of feature flags so the Rust compiler may reuse built artifacts effectively, reducing build time

### Decision

Test utilities should be organized following these rules:

1. Each crate must define its private and public test utilities in a `test` module
2. A test utility should be made public if it is reused in a child crate or in the crate's integration tests
3. Making a test utility public should not add any dependency to the crate
4. Private test utilities should be behind `cfg(test)`
5. Importing a public test utility should always require an import path that contains a `test` module, so their misuse
   in production code can be easily identified
6. No feature flag should be added to isolate test utilities from production code

Specific cases:

- Test doubles (fakes, dummies, mocks, stubs, etc.): should be located in a `test::double` module
- Builders of test data should be located in a `test::builder` module
- Extending a type with a test-only method should be done using extension traits located in the `test` module, which
  forces the import of a `test`-scoped symbol
  - Their name should be suffixed with `TestExtension`
  - Implementation of those traits should preferably be below the trait definition, but if some methods need to access
    private properties, then the implementation may be located below their extended type

#### Consequences

- Enhanced consistency in code organization
- Improved discoverability of test utilities
- Clearer distinction between production and test code
- Simplified maintenance of test utilities
- Child crates can reuse common test utilities when needed
- Feature flag usage is minimized, avoiding unnecessary recompilation

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

## 2. Remove Artifacts serialization support when compiling to WebAssembly

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

## 1. Record Architecture Decisions

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
