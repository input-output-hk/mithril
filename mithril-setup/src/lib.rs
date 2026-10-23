#![warn(missing_docs)]

//! A command line interface helping to setup Mithril environments.

mod command_context;
pub mod commands;
pub mod orchestrator;
pub mod signer;

pub use command_context::*;
