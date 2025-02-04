//! Tools to request metadata, like the current epoch or the stake distribution, from the Cardano

mod interface;
mod model;

#[cfg(test)]
pub use interface::MockChainObserver;
pub use interface::{ChainObserver, ChainObserverError};
pub use model::{
    ChainAddress, TxDatum, TxDatumBuilder, TxDatumError, TxDatumFieldTypeName, TxDatumFieldValue,
};

cfg_fs! {
    mod builder;
    mod cli_observer;
    mod pallas_observer;

    #[cfg(test)]
    mod test_cli_runner;

    pub use builder::{ChainObserverBuilder, ChainObserverType};
    pub use cli_observer::CliRunner;
    pub use cli_observer::{CardanoCliChainObserver, CardanoCliRunner};
    pub use pallas_observer::PallasChainObserver;
}

cfg_test_tools! {
    mod fake_observer;

    pub use fake_observer::FakeObserver;
}
