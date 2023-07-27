use crate::crypto_helper::ProtocolKey;
use mithril_stm::stm::StmSig;

/// Wrapper of [MithrilStm:StmSig](type@StmSig)
/// to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<StmSig>;
