use kes_summed_ed25519::kes::Sum6KesSig;
#[cfg(feature = "future_snark")]
use mithril_stm::VerificationKeyForSnark;
use mithril_stm::{
    AggregateSignature, AggregateVerificationKey, AggregateVerificationKeyForConcatenation,
    SingleSignature, VerificationKeyProofOfPossessionForConcatenation,
};

use crate::crypto_helper::{MKMapProof, MKProof, OpCert, ProtocolKey, ProtocolMembershipDigest};
use crate::entities::BlockRange;

/// Wrapper of [MithrilStm:VerificationKeyProofOfPossessionForConcatenation](type@VerificationKeyProofOfPossessionForConcatenation) to add serialization
/// utilities.
pub type ProtocolSignerVerificationKeyForConcatenation =
    ProtocolKey<VerificationKeyProofOfPossessionForConcatenation>;

/// Wrapper of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs) to add
/// serialization utilities.
pub type ProtocolSignerVerificationKeySignatureForConcatenation = ProtocolKey<Sum6KesSig>;

/// Wrapper of [MithrilStm:VerificationKeyForSnark](type@VerificationKeyForSnark) to add serialization
/// utilities.
#[cfg(feature = "future_snark")]
pub type ProtocolSignerVerificationKeyForSnark = ProtocolKey<VerificationKeyForSnark>;

/// Wrapper of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs) to add
/// serialization utilities.
#[cfg(feature = "future_snark")]
pub type ProtocolSignerVerificationKeySignatureForSnark = ProtocolKey<Sum6KesSig>;

/// Wrapper of [MithrilStm:SingleSignature](type@SingleSignature) to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<SingleSignature>;

/// Wrapper of [MithrilStm:AggregateSignature](enum@AggregateSignature) to add serialization utilities.
pub type ProtocolMultiSignature = ProtocolKey<AggregateSignature<ProtocolMembershipDigest>>;

/// Wrapper of [OpCert] to add serialization utilities.
pub type ProtocolOpCert = ProtocolKey<OpCert>;

/// Wrapper of [MithrilStm:AggregateVerificationKey](struct@AggregateVerificationKey).
pub type ProtocolAggregateVerificationKey = AggregateVerificationKey<ProtocolMembershipDigest>;

/// Wrapper of [MithrilStm:AggregateVerificationKeyForConcatenation](struct@AggregateVerificationKeyForConcatenation).
pub type ProtocolAggregateVerificationKeyForConcatenation =
    ProtocolKey<AggregateVerificationKeyForConcatenation<ProtocolMembershipDigest>>;

/// Wrapper of [MKProof] to add serialization utilities.
pub type ProtocolMkProof = ProtocolKey<MKMapProof<BlockRange>>;

impl_codec_and_type_conversions_for_protocol_key!(
    json_hex_codec => AggregateSignature<ProtocolMembershipDigest>, ed25519_dalek::VerifyingKey, ed25519_dalek::SigningKey, AggregateVerificationKeyForConcatenation<ProtocolMembershipDigest>,
        MKProof, VerificationKeyProofOfPossessionForConcatenation, Sum6KesSig, OpCert, SingleSignature
);

impl_codec_and_type_conversions_for_protocol_key!(
    bytes_hex_codec => ed25519_dalek::Signature
);

#[cfg(feature = "future_snark")]
impl_codec_and_type_conversions_for_protocol_key!(
    bytes_hex_codec => VerificationKeyForSnark
);
