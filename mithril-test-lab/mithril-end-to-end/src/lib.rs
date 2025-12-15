pub mod assertions;
mod devnet;
mod end_to_end_spec;
mod mithril;
mod run_only;
pub mod stress_test;
mod utils;

use clap::ValueEnum;
pub use devnet::*;
pub use end_to_end_spec::Spec;
pub use mithril::*;
pub use run_only::RunOnly;
pub use utils::{CompatibilityChecker, CompatibilityCheckerError, NodeVersion};
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DmqNodeFlavor {
    Haskell,
    Fake,
}
