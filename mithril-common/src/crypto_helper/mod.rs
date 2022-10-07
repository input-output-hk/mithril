//! Tools and types to abstract the use of the [Mithril Core library](https://mithril.network/mithril-core/doc/mithril/index.html)

mod cardano;
mod codec;
mod conversions;
mod genesis;
#[cfg(any(test, feature = "test_only"))]
pub mod tests_setup;
mod types;

#[cfg(any(test, feature = "test_only"))]
pub use cardano::ColdKeyGenerator;
pub use cardano::{KESPeriod, OpCert, SerDeShelleyFileFormat};
pub use codec::*;
pub use genesis::{ProtocolGenesisError, ProtocolGenesisSigner, ProtocolGenesisVerifier};
pub use types::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
