use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;

////////// Temporary trait, to understand the flaky tests ///////////

use mithril::{multi_sig::*, stm::*};

pub trait BytesConv: Sized {
    fn from_byte(bytes: &[u8]) -> Result<Self, String>;
    fn to_byte(&self) -> Vec<u8>;
}

impl BytesConv for VerificationKeyPoP {
    fn from_byte(bytes: &[u8]) -> Result<Self, String> {
        VerificationKeyPoP::from_bytes(bytes).map_err(|_| format!("Failed"))
    }

    fn to_byte(&self) -> Vec<u8> {
        self.to_bytes().to_vec()
    }
}
impl BytesConv for StmInitializer {
    fn from_byte(bytes: &[u8]) -> Result<Self, String> {
        StmInitializer::from_bytes(bytes).map_err(|_| format!("Failed"))
    }

    fn to_byte(&self) -> Vec<u8> {
        self.to_bytes().to_vec()
    }
}

impl BytesConv for StmSig<blake2::Blake2b> {
    fn from_byte(bytes: &[u8]) -> Result<Self, String> {
        StmSig::<blake2::Blake2b>::from_bytes(bytes).map_err(|_| format!("Failed"))
    }

    fn to_byte(&self) -> Vec<u8> {
        self.to_bytes().to_vec()
    }
}

impl BytesConv for StmAggrSig<blake2::Blake2b> {
    fn from_byte(bytes: &[u8]) -> Result<Self, String> {
        StmAggrSig::<blake2::Blake2b>::from_bytes(bytes).map_err(|_| format!("Failed"))
    }

    fn to_byte(&self) -> Vec<u8> {
        self.to_bytes().to_vec()
    }
}

impl BytesConv for StmAggrVerificationKey<blake2::Blake2b> {
    fn from_byte(bytes: &[u8]) -> Result<Self, String> {
        StmAggrVerificationKey::<blake2::Blake2b>::from_bytes(bytes).map_err(|_| format!("Failed"))
    }

    fn to_byte(&self) -> Vec<u8> {
        self.to_bytes().to_vec()
    }
}

////////// Temporary trait, to understand the flaky tests ///////////

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: &T) -> Result<String, String>
where
    T: BytesConv,
{
    Ok(from.to_byte().encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: &str) -> Result<T, String>
where
    T: BytesConv,
{
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    T::from_byte(&from_vec)
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
            key_encode_hex(&verification_key).expect("unexpected hex encoding error");
        let verification_key_restored =
            key_decode_hex(&verification_key_hex).expect("unexpected hex decoding error");
        assert_eq!(verification_key, verification_key_restored);
    }
}
