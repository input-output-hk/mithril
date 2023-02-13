//! Module dedicated to EraReaderAdapter implementations.
mod bootstrap;
mod cardano_chain;
mod dummy;

pub use bootstrap::BootstrapAdapter as EraReaderBootstrapAdapter;
pub use cardano_chain::CardanoChainAdapter as EraReaderCardanoChainAdapter;
pub use dummy::DummyAdapter as EraReaderDummyAdapter;
