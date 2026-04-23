//! Test utilities
//!
//! A collection of testing utilities and helpers:
//!
//! * `mk_extensions`: Extension traits adding test-specific methods for [crate::MKMap], [crate::MKTree], and [crate::MKProof]
//! * `range`: A simple range type used as a [crate::MKMapKey] in tests and benchmarks
//!

mod mk_extensions;
mod range;

pub use mk_extensions::{MKMapTestExtension, MKProofTestExtension, MKTreeTestExtension};
pub use range::TestRange;
