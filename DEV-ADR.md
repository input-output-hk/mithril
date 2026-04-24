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

### 8. Running slow tests on CI conditionally

**Date:** 2025-04-23
**Status:** Accepted

#### Context

Some tests are inherently too slow to run on every CI execution — typically because they rely on complex logic or
cryptography that cannot be optimized further.
This was significantly slowing down the CI pipeline, in some cases doubling the total test run time and hurting
developer productivity.

#### Decision

##### 1. Identify slow tests

A test **must** be categorized as "slow" if it **consistently** takes more than **30 seconds** to run on CI.

Because run times can vary significantly based on environmental conditions (e.g. machine performance or load),
developers **may** also categorize a test as "slow" at their discretion if it takes more than **15 seconds** on their local
machine.

The CI should automatically flag tests exceeding the 30-second threshold using tools such as cargo
nextest [slow test output](https://nexte.st/docs/features/slow-tests/#slow-tests), so that slow tests can be identified
and categorized.

##### 2. Marking tests as "slow"

To mark a test as "slow":

1. Move it into a dedicated `slow` submodule, placed at the end of the `tests` module:

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn normal_test() { }

    mod slow {
        use super::*;

        #[test]
        fn heavy_test() {
            // heavy test logic
        }
    }
}
```

2. If the slow test belongs to a package not yet listed in [`filter-slow-tests.sh`](.github/workflows/scripts/filter-slow-tests.sh),
   or if its source path is not already covered by an existing entry, update the script's slow test entries accordingly.
   See the script's [README.md](./docs/devbook/filter-slow-tests/README.md) for more details.

> [!CAUTION]
> If a test is moved to a `slow` submodule but the script is not updated, **it will never run on CI**.

##### 3. Running slow tests selectively

Both developers and CI should run slow tests only when a related source path has changed or when explicitly requested.

Use the [`filter-slow-tests.sh`](.github/workflows/scripts/filter-slow-tests.sh) script to generate a
`cargo nextest` filter expression that selectively includes slow tests based on which source paths have changed.

The CI must run this script automatically to keep total test run times low.

The CI must also allow developers to explicitly request a full test run, regardless of which source paths have changed,
via the `run-slow-tests` Pull Request label.

#### Consequences

- Faster testing feedback loop for developers
- Faster CI runs in the common case
- Developers can easily skip slow tests in local runs:
  - For `cargo test`: `cargo test -- --skip slow::`
  - For `cargo nextest`: `cargo nextest run -E "not test(#*slow::*)"`
- Developers can run only slow tests when needed

```shell
cargo nextest run --workspace --profile ci -E "$(.github/workflows/scripts/filter-slow-tests.sh)"
```

- [`filter-slow-tests.sh`](.github/workflows/scripts/filter-slow-tests.sh) becomes a maintenance dependency.
  Stale or missing entries will silently cause slow tests to be excluded from CI runs. Entries should be reviewed
  whenever a slow test is added, renamed, or moved.
- Marking a test as "slow" and keeping the script in sync introduces a small but ongoing maintenance overhead.

---

### 7. Do not expose arithmetic wrapper types to WebAssembly

**Date:** 2025-04-15
**Status:** Accepted

#### Context

As specified in [DEV-ADR-6](#6-guidelines-for-arithmetic-wrapper-types), arithmetic wrapper types enforce type safety for
numeric values.

However, when such types are exposed to WebAssembly, `wasm_bindgen` represents their inner tuple field as `.0` in
JavaScript, forcing callers to access the underlying primitive through that field:

```javascript
const block_number = cardano_transaction_proof.latest_block_number;
// Expected usage:
console.log(block_number);
// Actual usage:
console.log(block_number.0);
```

The `.0` accessor is an escape hatch for Rust interop, not an intended public interface. Exposing it to JavaScript
consumers makes the API awkward and leaks implementation details.

#### Decision

1. **Do not annotate arithmetic wrapper types with `#[wasm_bindgen]`**: The attribute should be absent from all wrapper
   types so they are never exposed as JavaScript classes.

2. **Hide arithmetic wrapper fields on exposed types**: On any `#[wasm_bindgen]` struct that holds an arithmetic wrapper
   field, mark that field with `#[cfg_attr(target_family = "wasm", wasm_bindgen(skip))]` to prevent it from being
   directly accessible in JavaScript.

3. **Expose the value through a getter instead**: Provide a `#[wasm_bindgen(getter)]` method that returns the underlying
   primitive type, scoped to `#[cfg(target_family = "wasm")]` so Rust code continues to access the field directly.

```Rust
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(target_family = "wasm", wasm_bindgen)]
pub struct ExampleType {
    #[cfg_attr(target_family = "wasm", wasm_bindgen(skip))]
    pub block_number: BlockNumber,
}

#[cfg(target_family = "wasm")]
#[wasm_bindgen]
impl ExampleType {
    #[wasm_bindgen(getter)]
    pub fn block_number(&self) -> u64 {
        *self.block_number
    }
}
```

#### Consequences

- Arithmetic wrapper types remain internal to Rust and are never surfaced as JavaScript classes.
- JavaScript consumers receive plain primitives (e.g. `number`, `bigint`) from getters, keeping the API idiomatic and free
  of Rust-specific field naming conventions.

---

### 6. Guidelines for arithmetic wrapper types

**Date:** 2025-01-06
**Status:** Accepted

#### Context

Numeric values in the codebase often represent distinct domain concepts (e.g.,`BlockNumber`, `Epoch`, `SlotNumber`,
`KesPeriod`, `KesEvolutions`). Using raw primitive types makes it easy to accidentally mix unrelated values, leading
to subtle bugs that the compiler cannot catch.

#### Decision

When wrapping arithmetic types, follow these guidelines:

1. **Restrict direct conversions**: Do not implement `From`/`Into` traits for conversions between the wrapper and
   primitive types. This prevents accidental escaping from the wrapper and forces explicit usage.

2. **Use constructor notation**: Prefer the `Type(value)` pattern for creating instances (e.g., `KesPeriod(42)`).
   This is more readable than `.into()` and makes the intent explicit.

3. **Implement arithmetic traits selectively**: Only implement arithmetic operations (`Add`, `Sub`, etc.) that make
   semantic sense for the domain concept.

4. **Exception for persistence**: Fallible conversions to `i64` (`TryFrom`) may be implemented when required by the
   persistence layer, as explicit wrapper usage there provides limited benefit.

#### Consequences

- The compiler enforces type safety, preventing accidental mixing of unrelated numeric values.
- Code is more readable with explicit `Type(value)` notation.
- Developers must consciously decide when to escape the wrapper, making type boundaries intentional.

---

### 5. Guidelines for writing useful log messages, error context, or error structure

**Date:** 2025-07-25
**Status:** Accepted

#### Context

Some errors and logs currently lack enough context to understand the cause of an issue.

This is especially true for failures that occur during requests to or from external sources.

At the same time, adding too much context can make logs noisy and hard to read, and can flood log storage with low-value
or sensitive data.

#### Decision

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

#### Consequences

- Logs are more readable and actionable.
- Errors are easier to understand and troubleshoot without routinely leaking sensitive data or producing excessive log volume.

---

### 4. Guidelines for crate test utilities

**Date:** 2025-07-25
**Status:** Accepted

#### Context

- Testing requires reusable utilities that may need to be shared across crates
- Test utilities should be isolated from production code while remaining accessible to child crates
- We need to minimize feature flags to optimize Rust compiler artifact reuse and reduce build times

#### Decision

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
