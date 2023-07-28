use mithril_stm::stm::StmSig;
use mithril_stm::stm::StmVerificationKeyPoP;

use crate::crypto_helper::ProtocolKey;

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@StmVerificationKeyPoP)
/// to add serialization utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<StmVerificationKeyPoP>;

impl ProtocolSignerVerificationKey {
    /// Output the key's bytes in memory
    pub fn to_bytes(&self) -> [u8; 192] {
        self.key().to_bytes()
    }
}

/// Wrapper of [MithrilStm:StmSig](type@StmSig)
/// to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<StmSig>;

impl_from_to_stm_types_for_protocol_key!(StmVerificationKeyPoP, StmSig);
