//! Tools to request metadata, like the current epoch or the stake distribution, from the Cardano

mod cli_observer;
#[cfg(any(test, feature = "test_only"))]
mod fake_observer;
mod interface;
mod model;

pub use cli_observer::{CardanoCliChainObserver, CardanoCliRunner};
#[cfg(any(test, feature = "test_only"))]
pub use fake_observer::FakeObserver;
pub use interface::{ChainObserver, ChainObserverError, MockChainObserver};
pub use model::{
    ChainAddress, TxDatum, TxDatumBuilder, TxDatumError, TxDatumFieldTypeName, TxDatumFieldValue,
};
