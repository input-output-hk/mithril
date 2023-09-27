---
slug: 6
title: |
  6. Errors implementation Standard
authors:
- name: Mithril Team
tags: [Draft]
date: 2023-09-27
---

## Status

Draft

## Context

Error handling is difficult with Rust:
- Many ways of implementing them with different crates ([`thiserror`](https://crates.io/crates/thiserror), [`anyhow`](https://crates.io/crates/anyhow), ...)
- No exception like handling of errors
- No stack trace or context available by default
- Backtrace uniquely when a panic occurs and if `RUST_BACKTRACE` environment variable is set to `1` or `full`

We think the errors handling should be done in a consistent way in the project. 
Thus we have worked on a standardization of their implementation and tried to apply it to the whole repository.
This has enabled us to have a clear vision of the do and don't that we intend to summarize in this ADR.

## Decision

_Therefore_

* We have decided to use `thiserror` and `anyhow` crates to implement the errors:
  * [`thiserror`](https://crates.io/crates/thiserror) is used to create module or domain errors that come from our developments and can be easily identified (as they are strongly typed).
  * [`anyhow`](https://crates.io/crates/anyhow) is used to add a context to an error triggered by a sub-system. The context is a convenient way to get 'stack trace' like debug information.

Here is a [Rust playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=bf667c443696beb90106f6ae627a57b9) that summarizes the usage of `thiserror`:

```rust
#[allow(unused_imports)]
use anyhow::{anyhow, Context, Result}; // 1.0.71
use thiserror::Error; // 1.0.43

#[derive(Error, Debug)]
#[error("Codec error: {msg}")]
pub struct CodecError {
    msg: String,
    #[source] // optional if field name is `source`
    source: anyhow::Error,
}

#[derive(Error, Debug)]
pub enum DomainError {
    #[error("Error with codec: {0:?}")]
    CodecWithOnlyDebug(CodecError),

    #[error("Error with codec")]
    CodecWithSource(#[source] CodecError),

    #[error("Error with codec: {0}")]
    CodecWithoutAnything(CodecError),

    #[error("Anyhow error: {0:?}")]
    AnyhowWrapWithOnlyDebug(anyhow::Error),

    #[error("Anyhow error")]
    AnyhowWrapWithSource(#[source] anyhow::Error),

    #[error("Anyhow error: {0}")]
    AnyhowWrapWithoutAnything(anyhow::Error),
}

fn anyhow_result() -> Result<()> {
    "invalid_number"
        .parse::<u64>()
        .map(|_| ())
        .with_context(|| "Reading database failure")
}

fn thiserror_struct() -> Result<(), CodecError> {
    Err(CodecError {
        msg: "My message".to_string(),
        source: anyhow!("Could not decode config"),
    })?;
    Ok(())
}

fn print_error(title: &str, error: anyhow::Error) {
    println!("{title:-^80}");
    println!("{error:?}\n",);
}

fn main() {
    println!("1 - Printing errors from enum variant that contains a error struct\n");
    // Debug the inner error struct: "normal" debug without the anyhow touch
    print_error(
        "DomainError::CodecWithOnlyDebug",
        anyhow!(DomainError::CodecWithOnlyDebug(
            thiserror_struct().unwrap_err()
        )),
    );
    // marking the inner error struct as source: anyhow will be able to make a
    // stacktrace out of this error. Nice !
    print_error(
        "DomainError::CodecWithSource",
        anyhow!(DomainError::CodecWithSource(
            thiserror_struct().unwrap_err()
        )),
    );
    // without debugging the inner error: only show the error text
    print_error(
        "DomainError::CodecWithoutAnything",
        anyhow!(DomainError::CodecWithoutAnything(
            thiserror_struct().unwrap_err()
        )),
    );

    println!("\n2 - Printing errors from enum variant that contains a anyhow error\n");
    // using only debug: the first two errors of the stack will be merged
    print_error(
        "DomainError::AnyhowWrapWithOnlyDebug",
        anyhow!(DomainError::AnyhowWrapWithOnlyDebug(
            anyhow_result().with_context(|| "context").unwrap_err()
        )),
    );
    // using #[source] attribute: each error of the stack will have a line
    print_error(
        "DomainError::AnyhowWrapWithSource",
        anyhow!(DomainError::AnyhowWrapWithSource(
            anyhow_result().with_context(|| "context").unwrap_err()
        )),
    );
    // without debug nor source: only the uppermost error is print
    print_error(
        "DomainError::AnyhowWrapWithoutAnything",
        anyhow!(DomainError::AnyhowWrapWithoutAnything(
            anyhow_result().with_context(|| "context").unwrap_err()
        )),
    );
}

```

Which will output errors this way:

```
1 - Printing errors from enum variant that contains a error struct

------------------------DomainError::CodecWithOnlyDebug-------------------------
Error with codec: CodecError { msg: "My message", source: Could not decode config }

--------------------------DomainError::CodecWithSource--------------------------
Error with codec

Caused by:
    0: Codec error: My message
    1: Could not decode config

-----------------------DomainError::CodecWithoutAnything------------------------
Error with codec: Codec error: My message


2 - Printing errors from enum variant that contains a anyhow error

----------------------DomainError::AnyhowWrapWithOnlyDebug----------------------
Anyhow error: context

Caused by:
    0: Reading database failure
    1: invalid digit found in string

-----------------------DomainError::AnyhowWrapWithSource------------------------
Anyhow error

Caused by:
    0: context
    1: Reading database failure
    2: invalid digit found in string

---------------------DomainError::AnyhowWrapWithoutAnything---------------------
Anyhow error: context
```

Here is a [Rust playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=90f962ab001d2ea0321fc5da0d4ec0f1) that summarizes the usage of the `context` feature form `anyhow`: 

```rust
#[allow(unused_imports)]
use anyhow::{anyhow, Context, Result}; // 1.0.71

fn read_db() -> Result<()> {
    "invalid_number"
        .parse::<u64>()
        .map(|_| ())
        .with_context(|| "Reading database failure")
}

fn do_work() -> Result<()> {
    read_db().with_context(|| "Important work failed while reading database")
}

fn do_service_work() -> Result<()> {
    do_work().with_context(|| "Service could not do the important work")
}

fn main() {
    let error = do_service_work().unwrap_err();

    println!("Error string:\n {error}\n\n");
    println!("Error debug:\n {error:?}\n\n");
    println!("Error pretty:\n {error:#?}\n\n");
}

```

Which will output errors this way:
```
Error string:
 Service could not do the important work


Error debug:
 Service could not do the important work

Caused by:
    0: Important work failed while reading database
    1: Reading database failure
    2: invalid digit found in string


Error pretty:
 Error {
    context: "Service could not do the important work",
    source: Error {
        context: "Important work failed while reading database",
        source: Error {
            context: "Reading database failure",
            source: ParseIntError {
                kind: InvalidDigit,
            },
        },
    },
}
```

## Consequences

* We have defined the following aliases that should be used by default:
    * `StdResult`: the default result that should be returned by a function (unless a more specific type is required).
    * `StdError`: the default error that should be used (unless a more specific type is required).

```rust
/* Code extracted from mithril-common::lib.rs */
/// Generic error type
pub type StdError = anyhow::Error;

/// Generic result type
pub type StdResult<T> = anyhow::Result<T, StdError>;
```

* The function that returns an error from a sub-system should systematically add a context to the error with the `with_context` method, in order to provide clear stack traces and ease debugging.

* When printing an `StdError` we should use the debug format without the pretty modifier, ie: 
```rust
println!("Error debug:\n {error:?}\n\n");
```

* When wrapping an error in a `thiserror` enum variant we should use the `source` attribute that will provide a clearer stack trace:
```rust
/// Correct usage with `source` attribute
#[derive(Error, Debug)]
pub enum DomainError {
    #[error("Anyhow error")]
    AnyhowWrapWithSource(#[source] StdError),
}
```

```rust
/// Incorrect usage without `source` attribute
#[derive(Error, Debug)]
pub enum DomainError {
    #[error("Anyhow error: {0}")]
    AnyhowWrapWithoutAnything(StdError),
}
```

* Here are some tips on how to discriminate between creating a new error using `thiserror` or using an `StdResult`:
  * If you raise an anyhow error which only contains a string this means that you are creating a new error that doesn't come from a sub-system. In that case you should create a type using `thiserror` intead, ie:
```rust
// Avoid
return Err(anyhow!("my new error"));

// Prefer
#[derive(Debug,Error)]
pub enum MyError {
  MyNewError
}
return Err(MyError::MyNewError);
```

  * (*Still undecided*) You should avoid wrapping a `StdError` in a `thiserror` type. This __breaks__ the stack trace and makes it really difficult to retrieve the innermost errors using `downcast_ref`. When the `thiserror` type is itself wrapped in a `StdError` afterward, you would have to `downcast_ref` twice: first to get the `thiserror` type and then to get the innermost error. 
  This should be restricted to the topmost errors of our system (ie the state machine errors).
