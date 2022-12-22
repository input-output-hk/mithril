//! Test utilities
//!
//! They contains:
//! * A Open Api Spec tester
//! * Some precomputed fake data
//! * A builder of [MithrilFixture] to generate signers alongside a stake distribution
//!
//! Note: Most of those tools are behind the `test_only` features.

#[cfg(any(test, feature = "test_only"))]
pub mod apispec;
#[cfg(any(test, feature = "test_only"))]
pub mod fake_data;
#[cfg(any(test, feature = "test_only"))]
mod fixture_builder;
mod mithril_fixture;

#[cfg(any(test, feature = "test_only"))]
pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use mithril_fixture::{MithrilFixture, SignerFixture};
