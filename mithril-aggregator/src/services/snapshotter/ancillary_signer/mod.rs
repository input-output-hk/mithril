//! Tools for signing generating signed manifests of ancillary data.
mod interface;
mod with_secret_key;

pub use interface::*;
pub use with_secret_key::*;
