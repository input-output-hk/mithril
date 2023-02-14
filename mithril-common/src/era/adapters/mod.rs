//! Module dedicated to EraReaderAdapter implementations.
mod bootstrap;
mod builder;
mod cardano_chain;
mod dummy;

pub use bootstrap::BootstrapAdapter as EraReaderBootstrapAdapter;
pub use builder::{AdapterBuilder as EraReaderAdapterBuilder, AdapterType as EraReaderAdapterType};
pub use cardano_chain::CardanoChainAdapter as EraReaderCardanoChainAdapter;
pub use dummy::DummyAdapter as EraReaderDummyAdapter;
