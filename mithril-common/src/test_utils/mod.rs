//! Test utilities
//!
//! They contains:
//! * A Open Api Spec tester
//! * Some precomputed fake data and keys
//! * A builder of [MithrilFixture] to generate signers alongside a stake distribution
//!

#[cfg(feature = "apispec")]
pub mod apispec;

pub mod fake_data;
pub mod fake_keys;

mod fixture_builder;
mod mithril_fixture;

#[cfg(feature = "test_http_server")]
pub mod test_http_server;

pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use mithril_fixture::{MithrilFixture, SignerFixture};
