//! Module dedicated to [EraReaderAdapter] implementations.
mod bootstrap;
mod dummy;

pub use bootstrap::BootstrapAdapter;
pub use dummy::DummyAdapter;
