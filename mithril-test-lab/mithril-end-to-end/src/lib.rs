pub mod assertions;
mod devnet;
mod end_to_end_spec;
mod mithril;
mod run_only;
pub mod stress_test;
mod utils;

pub use devnet::*;
pub use end_to_end_spec::Spec;
pub use mithril::*;
pub use run_only::RunOnly;
pub use utils::{CompatibilityChecker, CompatibilityCheckerError, NodeVersion};

use clap::ValueEnum;
/// The flavor of DMQ node to use in the tests.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DmqNodeFlavor {
    /// Haskell implementation of DMQ.
    Haskell,
    /// Fake implementation of DMQ.
    Fake,
}

/// The type of STM aggregate signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum AggregateSignatureType {
    /// Concatenation proof system.
    #[value(name = "Concatenation")]
    Concatenation,
    /// SNARK proof system.
    #[value(name = "Snark")]
    Snark,
}

impl std::fmt::Display for AggregateSignatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggregateSignatureType::Concatenation => write!(f, "Concatenation"),
            AggregateSignatureType::Snark => write!(f, "Snark"),
        }
    }
}
