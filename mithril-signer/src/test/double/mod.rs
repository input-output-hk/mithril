//! Test doubles
//!
//! Enable unit testing with controlled inputs and predictable behavior.

mod dummies;
#[cfg(test)]
mod signer_registration_publisher;
#[cfg(test)]
mod signer_registration_retriever;

#[cfg(test)]
pub use signer_registration_publisher::*;
#[cfg(test)]
pub use signer_registration_retriever::*;
