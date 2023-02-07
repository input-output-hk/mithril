//! Module dedicated to EraReaderAdapter implementations.
mod bootstrap;
mod cardano_chain;
mod dummy;

pub use bootstrap::BootstrapAdapter as EraReaderBootstrapAdapter;
pub use cardano_chain::CardanoChainAdapter;
pub use dummy::DummyAdapter as EraReaderDummyAdapter;
