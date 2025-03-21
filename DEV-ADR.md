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
