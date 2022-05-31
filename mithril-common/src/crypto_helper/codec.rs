use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;

////////// Temporary trait, to understand the flaky tests ///////////

pub trait BytesConv: Sized {
    fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
        Self::from_bytes(bytes).map_err(|_| "Failed to convert from bytes".to_owned())
    }

    fn to_bytes(&self) -> Vec<u8> {
        self.to_bytes()
    }
}

impl BytesConv for mithril::multi_sig::VerificationKeyPoP {}
impl BytesConv for &mithril::multi_sig::VerificationKeyPoP {}
impl BytesConv for mithril::stm::StmInitializer {}
impl BytesConv for &mithril::stm::StmInitializer {}
impl BytesConv for mithril::stm::StmSig<blake2::Blake2b> {}
impl BytesConv for &mithril::stm::StmSig<blake2::Blake2b> {}
impl BytesConv for mithril::stm::StmAggrSig<blake2::Blake2b> {}
impl BytesConv for &mithril::stm::StmAggrSig<blake2::Blake2b> {}
impl BytesConv for mithril::stm::StmAggrVerificationKey<blake2::Blake2b> {}
impl BytesConv for &mithril::stm::StmAggrVerificationKey<blake2::Blake2b> {}


////////// Temporary trait, to understand the flaky tests ///////////

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: T) -> Result<String, String>
where
    T: BytesConv,
{
    Ok(from.to_bytes()
        .encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: &str) -> Result<T, String>
where
    T: BytesConv,
{
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    T::from_bytes(&from_vec)
}

#[cfg(test)]
pub mod tests {
    use super::super::tests_setup::*;
    use super::super::types::*;
    use super::*;

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn test_key_encode_decode_hex() {
        let protocol_params = setup_protocol_parameters();
        let party_id = 123;
        let stake = 100;
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_initializer: ProtocolInitializer =
            ProtocolInitializer::setup(protocol_params, party_id, stake, &mut rng);
        let verification_key: ProtocolSignerVerificationKey =
            protocol_initializer.verification_key();
        let verification_key_hex =
            key_encode_hex(verification_key).expect("unexpected hex encoding error");
        let verification_key_restored =
            key_decode_hex(&verification_key_hex).expect("unexpected hex decoding error");
        assert_eq!(verification_key, verification_key_restored);
    }
}
