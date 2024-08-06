//! Tools and types to abstract the use of the [Mithril STM library](https://mithril.network/rust-doc/mithril_stm/index.html)

mod cardano;
mod codec;
mod conversions;
mod era;
mod genesis;
mod merkle_map;
mod merkle_tree;
mod types;

cfg_test_tools! {
    pub mod tests_setup;
}

cfg_random! {
    pub use cardano::ColdKeyGenerator;
}

pub use cardano::{
    KESPeriod, OpCert, ProtocolInitializerErrorWrapper, ProtocolRegistrationErrorWrapper,
    SerDeShelleyFileFormat, Sum6KesBytes,
};
pub use codec::*;
pub use era::{
    EraMarkersSigner, EraMarkersVerifier, EraMarkersVerifierError, EraMarkersVerifierSecretKey,
    EraMarkersVerifierSignature, EraMarkersVerifierVerificationKey,
};
pub use genesis::{ProtocolGenesisError, ProtocolGenesisSigner, ProtocolGenesisVerifier};
pub use merkle_map::{MKMap, MKMapKey, MKMapNode, MKMapProof, MKMapValue};
pub use merkle_tree::{
    MKProof, MKTree, MKTreeLeafIndexer, MKTreeNode, MKTreeStoreInMemory, MKTreeStorer,
};
pub use types::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
