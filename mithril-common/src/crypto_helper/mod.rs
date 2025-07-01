//! Tools and types to abstract the use of the [Mithril STM library](https://mithril.network/rust-doc/mithril_stm/index.html)

mod cardano;
mod codec;
mod conversions;
pub mod ed25519;
mod ed25519_alias;
mod merkle_map;
mod merkle_tree;
mod types;

cfg_test_tools! {
    pub mod tests_setup;
}

pub use cardano::ColdKeyGenerator;

pub use cardano::{
    KesPeriod, KesSigner, KesSignerStandard, KesVerifier, KesVerifierStandard, KesVerifyError,
    OpCert, ProtocolInitializerErrorWrapper, ProtocolRegistrationErrorWrapper,
    SerDeShelleyFileFormat, Sum6KesBytes,
};
cfg_test_tools! {
    pub use cardano::KesSignerFake;
}
pub use codec::*;
pub use ed25519_alias::{era::*, genesis::*, manifest::*};
pub use merkle_map::{MKMap, MKMapKey, MKMapNode, MKMapProof, MKMapValue};
pub use merkle_tree::{
    Bytes, MKProof, MKTree, MKTreeLeafIndexer, MKTreeLeafPosition, MKTreeNode, MKTreeStoreInMemory,
    MKTreeStorer,
};
pub use types::*;

/// The current protocol version
pub const PROTOCOL_VERSION: ProtocolVersion = "0.1.0";
