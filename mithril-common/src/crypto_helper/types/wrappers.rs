use kes_summed_ed25519::kes::Sum6KesSig;
use mithril_stm::{
    AggregateSignature, AggregateVerificationKey, SingleSignature, VerificationKeyProofOfPossession,
};

use crate::crypto_helper::{D, MKMapProof, MKProof, OpCert, ProtocolKey};
use crate::entities::BlockRange;

/// Wrapper of [MithrilStm:VerificationKeyProofOfPossession](type@VerificationKeyProofOfPossession) to add serialization
/// utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<VerificationKeyProofOfPossession>;

/// Wrapper of [KES:Sum6KesSig](https://github.com/input-output-hk/kes/blob/master/src/kes.rs) to add
/// serialization utilities.
pub type ProtocolSignerVerificationKeySignature = ProtocolKey<Sum6KesSig>;

/// Wrapper of [MithrilStm:SingleSignature](type@SingleSignature) to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<SingleSignature>;

/// Wrapper of [MithrilStm:AggregateSignature](enum@AggregateSignature) to add serialization utilities.
pub type ProtocolMultiSignature = ProtocolKey<AggregateSignature<D>>;

/// Wrapper of [OpCert] to add serialization utilities.
pub type ProtocolOpCert = ProtocolKey<OpCert>;

/// Wrapper of [MithrilStm:AggregateVerificationKey](struct@AggregateVerificationKey).
pub type ProtocolAggregateVerificationKey = ProtocolKey<AggregateVerificationKey<D>>;

/// Wrapper of [MKProof] to add serialization utilities.
pub type ProtocolMkProof = ProtocolKey<MKMapProof<BlockRange>>;

impl_codec_and_type_conversions_for_protocol_key!(
    json_hex_codec => AggregateSignature<D>, ed25519_dalek::VerifyingKey, ed25519_dalek::SigningKey, AggregateVerificationKey<D>,
        MKProof, VerificationKeyProofOfPossession, Sum6KesSig, OpCert, SingleSignature
);

impl_codec_and_type_conversions_for_protocol_key!(
    bytes_hex_codec => ed25519_dalek::Signature
);
