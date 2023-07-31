use mithril_stm::stm::{StmAggrSig, StmSig, StmVerificationKeyPoP};

use crate::crypto_helper::{ProtocolKey, D};

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@StmVerificationKeyPoP) to add serialization
/// utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<StmVerificationKeyPoP>;

impl ProtocolSignerVerificationKey {
    /// Output the key's bytes in memory
    pub fn to_bytes(&self) -> [u8; 192] {
        self.key().to_bytes()
    }
}

/// Wrapper of [MithrilStm:StmSig](type@StmSig) to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<StmSig>;

/// Wrapper of [MithrilStm:StmAggrSig](struct@StmAggrSig) to add serialization utilities.
pub type ProtocolMultiSignature = ProtocolKey<StmAggrSig<D>>;

impl_from_to_stm_types_for_protocol_key!(StmVerificationKeyPoP, StmSig, StmAggrSig<D>);
