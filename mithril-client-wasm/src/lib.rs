#![warn(missing_docs)]
//! Implementation of the 'mithril-client' library in WASM
mod client_wasm;

pub use client_wasm::MithrilClient;

#[cfg(test)]
mod test_data;
