//! Test doubles
//!
//! Enable unit testing with controlled inputs and predictable behavior.

mod immutable_digester;
mod immutable_file_observer;

pub use immutable_digester::DumbImmutableDigester;
pub use immutable_file_observer::DumbImmutableFileObserver;
