//! Tools to request metadata, like the current epoch or the stake distribution, from the Cardano

mod cli_observer;
mod fake_observer;
mod interface;

pub use cli_observer::{CardanoCliChainObserver, CardanoCliRunner};
pub use fake_observer::FakeObserver;
pub use interface::{ChainObserver, ChainObserverError};
