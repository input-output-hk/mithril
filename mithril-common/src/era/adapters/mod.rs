//! Module dedicated to EraReaderAdapter implementations.
mod bootstrap;
mod dummy;

pub use bootstrap::BootstrapAdapter as EraReaderBootstrapAdapter;
pub use dummy::DummyAdapter as EraReaderDummyAdapter;
