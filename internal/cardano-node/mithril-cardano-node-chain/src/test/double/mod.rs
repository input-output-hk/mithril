//! Test doubles
//!
//! Enable unit testing of blockchain interactions with controlled inputs and predictable behavior.

mod block_scanner;
mod chain_observer;
mod chain_reader;

pub use block_scanner::{DumbBlockScanner, DumbBlockStreamer};
pub use chain_observer::FakeObserver;
pub use chain_reader::FakeChainReader;
