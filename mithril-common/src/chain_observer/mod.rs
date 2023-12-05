//! Tools to request metadata, like the current epoch or the stake distribution, from the Cardano

#[cfg(all(feature = "fs", feature = "random"))]
mod cli_observer;
#[cfg(feature = "test_tools")]
mod fake_observer;
mod interface;
mod model;
#[cfg(all(feature = "fs", feature = "random"))]
mod pallas_observer;

#[cfg(test)]
mod test_cli_runner;

#[cfg(test)]
pub use cli_observer::CliRunner;

#[cfg(all(feature = "fs", feature = "random"))]
pub use cli_observer::{CardanoCliChainObserver, CardanoCliRunner};
#[cfg(feature = "test_tools")]
pub use fake_observer::FakeObserver;
#[cfg(test)]
pub use interface::MockChainObserver;
pub use interface::{ChainObserver, ChainObserverError};
pub use model::{
    ChainAddress, TxDatum, TxDatumBuilder, TxDatumError, TxDatumFieldTypeName, TxDatumFieldValue,
};
#[cfg(all(feature = "fs", feature = "random"))]
pub use pallas_observer::PallasChainObserver;
