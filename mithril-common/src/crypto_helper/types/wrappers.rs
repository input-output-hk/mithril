use kes_summed_ed25519::kes::Sum6KesSig;
use mithril_stm::{SingleSignature, StmAggrSig, StmAggrVerificationKey, StmVerificationKeyPoP};

use crate::crypto_helper::{MKMapProof, MKProof, OpCert, ProtocolKey, D};
use crate::entities::BlockRange;

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@StmVerificationKeyPoP) to add serialization
/// utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<StmVerificationKeyPoP>;

/// Wrapper of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs) to add
/// serialization utilities.
pub type ProtocolSignerVerificationKeySignature = ProtocolKey<Sum6KesSig>;

/// Wrapper of [MithrilStm:StmSig](type@StmSig) to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<SingleSignature>;

/// Wrapper of [MithrilStm:StmAggrSig](struct@StmAggrSig) to add serialization utilities.
pub type ProtocolMultiSignature = ProtocolKey<StmAggrSig<D>>;

/// Wrapper of [OpCert] to add serialization utilities.
pub type ProtocolOpCert = ProtocolKey<OpCert>;

/// Wrapper of [MithrilStm:StmAggrVerificationKey](struct@StmAggrVerificationKey).
pub type ProtocolAggregateVerificationKey = ProtocolKey<StmAggrVerificationKey<D>>;

/// Wrapper of [MKProof] to add serialization utilities.
pub type ProtocolMkProof = ProtocolKey<MKMapProof<BlockRange>>;

impl_codec_and_type_conversions_for_protocol_key!(
    json_hex_codec => StmAggrSig<D>, ed25519_dalek::VerifyingKey, ed25519_dalek::SigningKey, StmAggrVerificationKey<D>,
        MKProof, StmVerificationKeyPoP, Sum6KesSig, OpCert
);

impl_codec_and_type_conversions_for_protocol_key!(
    bytes_hex_codec => SingleSignature, ed25519_dalek::Signature
);
