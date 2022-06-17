use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;

/// Encode key to hex helper
pub fn key_encode_hex<T>(from: T) -> Result<String, String>
where
    T: Serialize,
{
    Ok(serde_json::to_string(&from)
        .map_err(|e| format!("can't convert to hex: {}", e))?
        .encode_hex::<String>())
}

/// Decode key from hex helper
pub fn key_decode_hex<T>(from: &str) -> Result<T, String>
where
    T: DeserializeOwned,
{
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    serde_json::from_slice(from_vec.as_slice()).map_err(|e| format!("can't deserialize: {}", e))
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
        let stake = 100;
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_initializer: ProtocolInitializer =
            ProtocolInitializer::setup(protocol_params, stake, &mut rng);
        let verification_key: ProtocolSignerVerificationKey =
            protocol_initializer.verification_key();
        let verification_key_hex =
            key_encode_hex(verification_key).expect("unexpected hex encoding error");
        let verification_key_restored =
            key_decode_hex(&verification_key_hex).expect("unexpected hex decoding error");
        assert_eq!(verification_key, verification_key_restored);
    }
}
