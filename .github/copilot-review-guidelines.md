# Copilot Code Review Guidelines

## Rust

### Error Handling

- **Never use `panic!()`, `unwrap()`, `todo!()`, or `unimplemented!()` in production code.**
  They are only authorized in:
  - Test code (`#[cfg(test)]` modules and `#[test]` functions).
  - Cases where the `unwrap()` is provably safe, **and** the reason why it is safe is
    documented in a comment next to the call (e.g. a preceding guard clause or invariant).
- Use proper error handling instead: `Result<T, E>` return types, the `?` operator,
  `map_err`, `with_context`, or dedicated error types.

### Imports

Imports must appear at the top of the file (or at the top of a `#[cfg(test)]` module for
test-only imports) and follow this ordering, with a blank line between each group:

1. Standard library (`std`, `core`, `alloc`)
2. External dependencies (third-party crates)
3. Crate-internal imports (`crate::`)
4. Parent / sibling imports (`super::`, `self::`)

```rust
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::domain::model::Certificate;
use crate::services::SignerService;

use super::helper::build_test_fixture;
```

### Documentation

- All public functions, methods, structs, enums, enum variants, struct fields, traits, and
  type aliases **must** have a doc comment (`///` or `//!`).
- Simple private helper functions are exempt from this requirement.
- Test functions do **not** require doc comments.

### Naming

- Every identifier (function, variable, type, constant, module, test name, ...) must be
  **clearly readable and explicit** enough to understand its purpose without surrounding
  context.
- Prefer full words over abbreviations (e.g. `configuration` over `config`,
  `certificate` over `cert`) unless the abbreviation is a widely accepted domain term.
- Test function names should describe the behavior under test; do **not** prefix them with
  `test_`.

### Unsafe Code

- `unsafe` blocks are **forbidden** unless strictly necessary (e.g. FFI boundaries).
- Every `unsafe` block **must** include a `// SAFETY:` comment explaining why the invariants
  are upheld.

### Clippy and Compiler Warnings

- Code must compile with **no warnings**.
- `#[allow(dead_code)]` is **not permitted** in production code. If code is unused, remove
  it. The only exception is scaffolding during an active, in-progress implementation that
  will be completed in the same pull request.
- **Do not introduce new `TODO`, `FIXME`, or `HACK` comments.** If work is deferred, create
  a tracking issue and reference it instead.

### Feature Flags

- **Adding a new Cargo feature flag requires explicit justification** in the pull request
  description. Feature flags add long-term maintenance cost and combinatorial testing
  complexity; prefer compile-time configuration or runtime configuration when possible.

### Ownership and Borrowing

- Prefer borrowing (`&T`, `&str`) over owned types (`T`, `String`) in function parameters
  when the function does not need ownership.
- Avoid unnecessary `.clone()` calls. If cloning is required, ensure it is intentional and
  not a workaround for a borrow-checker issue that could be solved with better lifetimes.

### Type Conversions

- Use `From` / `Into` trait implementations for type conversions instead of ad-hoc
  conversion methods.
- Prefer `TryFrom` / `TryInto` when the conversion can fail.

### Constants and Magic Values

- Do not use hardcoded literal values (numbers, strings) in logic. Extract them into named
  constants or configuration values so their intent is clear and they are easy to update.

### Derive and Trait Implementations

- Use `#[derive(...)]` for standard traits (`Debug`, `Clone`, `PartialEq`, etc.) instead of
  writing manual implementations, unless custom behavior is specifically needed.

### Testing

- Test names must describe the **behavior** under test, not the function being called
  (e.g. `cant_sign_if_not_in_current_signers` over `test_sign`).
- Cover **edge cases** and failure paths, not only the happy path.
- Use mocks and dependency injection to isolate the unit under test.
- Tests should be deterministic and not depend on external state or ordering.

### Architecture Decision Records (ADRs)

All code changes **must** comply with the project's Architecture Decision Records.
Flag any violation during review.

#### Developer ADRs (`DEV-ADR.md`)

1. **Arithmetic wrapper types** — Use newtype wrappers for domain numerics (e.g.
   `BlockNumber`, `Epoch`). Do not implement `From`/`Into` to primitives; use `Type(value)`
   constructor notation. Only implement arithmetic traits that are semantically meaningful.
2. **Logging and error context** — Use structured logging with identifiers as fields, not
   embedded in the message. Never log secrets or large payloads. Use manual `Debug` impls to
   control output size and sensitivity.
3. **Crate test utilities** — Place test utilities in a dedicated `test` module. Use
   `test::double` for mocks/fakes/stubs, `test::builder` for data builders, and
   `TestExtension` suffix for test-only extension traits. No feature flags for test isolation.
4. **Dummy test doubles** — Implement the `Dummy` trait (similar to `Default`) for test
   doubles. Place implementations in `test::double::dummies`, or next to the type when
   non-public fields are involved.
5. **WebAssembly compatibility** — Do not use `typetag::serde` in code compiled to
   WebAssembly.

#### Project ADRs (`docs/website/adr/`)

6. **Structured logging** (ADR 2 / 7) — Emit structured JSON logs. Use stderr for CLI tools,
   stdout for long-running services.
7. **Date formatting** (ADR 5) — Use RFC 3339 with UTC for all date/time values.
8. **Error handling** (ADR 6) — Use `thiserror` for domain errors and `anyhow` for
   contextual wrapping.
9. **JSON message golden tests** (ADR 8) — Add golden tests for serialized JSON messages to
   ensure backward compatibility.
10. **Database migration squashing** (ADR 9) — Consolidate accumulated migrations into single
    equivalent migrations when appropriate.
11. **HTTP status codes** (ADR 10) — Use Mithril-specific status codes (450-499 client,
    550-599 server) for functional error cases.

### Clean Code and Idiomatic Rust

- Prefer small, focused functions with a single responsibility.
- Avoid deep nesting; use early returns and guard clauses.
- Favor iterators and combinators over manual loops when they improve clarity.
- Use exhaustive pattern matching; avoid catch-all `_ =>` arms when the variants are known
  and limited.
- Do not leave dead code, commented-out code, or unused imports.
- Avoid inline comments unless they explain a non-obvious decision, an opinionated choice,
  or a bug-fix workaround. The code itself should be readable without comments.
- Keep public APIs minimal; default to private visibility and widen only when needed.
- Use `#[must_use]` on functions whose return values should not be silently ignored.
- Prefer strong typing and newtypes over primitive obsession.
