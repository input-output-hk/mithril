//! Tools to request metadata, like the current epoch or the stake distribution, from the Cardano

pub mod adapters;
mod cli_observer;
mod fake_observer;
mod interface;
mod model;
mod pallas_observer;

pub use cli_observer::{CardanoCliChainObserver, CardanoCliRunner};
pub use fake_observer::FakeObserver;
#[cfg(test)]
pub use interface::MockChainObserver;
pub use interface::{ChainObserver, ChainObserverError};
pub use model::{
    ChainAddress, TxDatum, TxDatumBuilder, TxDatumError, TxDatumFieldTypeName, TxDatumFieldValue,
};
pub use pallas_observer::PallasObserver;
