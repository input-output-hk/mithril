//! Tools and types to abstract the use of the [Mithril STM library](https://mithril.network/rust-doc/mithril_stm/index.html)

mod cardano;
mod codec;
mod conversions;
mod era;
mod genesis;
pub mod tests_setup;
mod types;
mod verification_key;

pub use cardano::{
    ColdKeyGenerator, KESPeriod, OpCert, ProtocolInitializerErrorWrapper,
    ProtocolRegistrationErrorWrapper, SerDeShelleyFileFormat, Sum6KesBytes,
};
pub use codec::*;
pub use era::{
    EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierError, EraMarkersVerifierSecretKey,
    EraMarkersVerifierSignature, EraMarkersVerifierVerificationKey,
};
pub use genesis::{ProtocolGenesisError, ProtocolGenesisSigner, ProtocolGenesisVerifier};
pub use types::*;
pub use verification_key::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
