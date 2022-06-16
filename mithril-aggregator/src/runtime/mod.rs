mod error;
mod runner;
mod state_machine;

pub use error::RuntimeError;
pub use runner::{AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait};
pub use state_machine::*;
