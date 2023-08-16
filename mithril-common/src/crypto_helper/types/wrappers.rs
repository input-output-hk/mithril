use anyhow::Context;
use hex::{FromHex, ToHex};
use mithril_stm::stm::{StmAggrSig, StmSig, StmVerificationKeyPoP};

use crate::crypto_helper::{OpCert, ProtocolKey, ProtocolKeyCodec, D};
use crate::StdResult;

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@StmVerificationKeyPoP) to add serialization
/// utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<StmVerificationKeyPoP>;

/// Wrapper of [MithrilStm:StmSig](type@StmSig) to add serialization utilities.
pub type ProtocolSingleSignature = ProtocolKey<StmSig>;

/// Wrapper of [MithrilStm:StmAggrSig](struct@StmAggrSig) to add serialization utilities.
pub type ProtocolMultiSignature = ProtocolKey<StmAggrSig<D>>;

/// Wrapper of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ProtocolKey<ed25519_dalek::Signature>;

/// Wrapper of [OpCert] to add serialization utilities.
pub type ProtocolOpCert = ProtocolKey<OpCert>;

impl ProtocolGenesisSignature {
    /// Create an instance from a bytes hex representation
    pub fn from_bytes_hex(hex_string: &str) -> StdResult<Self> {
        let hex_bytes = Vec::from_hex(hex_string).with_context(|| {
            "Could not deserialize a ProtocolGenesisSignature from bytes hex string:\
            could not convert the encoded string to bytes."
        })?;

        Self::from_bytes(&hex_bytes)
    }

    /// Create an instance from a bytes representation
    pub fn from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let key = ed25519_dalek::Signature::from_bytes(bytes).with_context(|| {
            "Could not deserialize a ProtocolGenesisSignature from bytes hex string:\
            invalid bytes"
                .to_string()
        })?;

        Ok(Self { key })
    }

    /// Create a bytes hash representation of the key
    pub fn to_bytes_hex(&self) -> String {
        Self::key_to_bytes_hex(&self.key)
    }

    /// Create a bytes hash representation of the given key
    pub fn key_to_bytes_hex(key: &ed25519_dalek::Signature) -> String {
        key.to_bytes().encode_hex::<String>()
    }
}

impl ProtocolKeyCodec<ed25519_dalek::Signature> for ed25519_dalek::Signature {
    fn decode_key(encoded: &str) -> StdResult<ProtocolKey<ed25519_dalek::Signature>> {
        ProtocolGenesisSignature::from_bytes_hex(encoded)
    }

    fn encode_key(key: &ed25519_dalek::Signature) -> StdResult<String> {
        Ok(ProtocolGenesisSignature::key_to_bytes_hex(key))
    }
}

impl_codec_and_type_conversions_for_protocol_key!(
    json_hex_codec => StmVerificationKeyPoP, StmSig, StmAggrSig<D>, OpCert
);
impl_codec_and_type_conversions_for_protocol_key!(no_default_codec => ed25519_dalek::Signature);
