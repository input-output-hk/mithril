//! Test doubles
//!
//! Enable unit testing of blockchain interactions with controlled inputs and predictable behavior.

mod block_scanner;
mod chain_data_store;
mod chain_observer;
mod chain_reader;

pub use block_scanner::{DumbBlockScanner, DumbBlockStreamer};
pub use chain_data_store::{
    InMemoryBlockRangeRoot, InMemoryChainDataStore, InMemoryChainDataStoreBuilder,
};
pub use chain_observer::FakeChainObserver;
pub use chain_reader::FakeChainReader;
