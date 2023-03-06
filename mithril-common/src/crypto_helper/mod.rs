//! Tools and types to abstract the use of the [Mithril STM library](https://mithril.network/rust-doc/mithril_stm/index.html)

mod cardano;
mod codec;
mod conversions;
mod era;
mod genesis;
#[cfg(any(test, feature = "test_only"))]
pub mod tests_setup;
mod types;

#[cfg(any(test, feature = "test_only"))]
pub use cardano::ColdKeyGenerator;
pub use cardano::{KESPeriod, OpCert, SerDeShelleyFileFormat, Sum6KesBytes};
pub use codec::*;
pub use era::{
    EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierError, EraMarkersVerifierSecretKey,
    EraMarkersVerifierSignature, EraMarkersVerifierVerificationKey,
};
pub use genesis::{ProtocolGenesisError, ProtocolGenesisSigner, ProtocolGenesisVerifier};
pub use types::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
