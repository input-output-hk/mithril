//! Dependency injection module.
//! This module provides tools to initialize and share resources and services
//! amongst different threads.
//!
//! It takes all its inputs from the configuration which should combine inputs from:
//!
//!  * environment
//!  * command line
//!  * configuration files
//!  * default values
//!
//! The Builder ensure every services has required dependencies to build and
//! provide services containers for each sub process.
mod builder;
mod error;

pub use builder::*;
pub use error::*;
