use super::types::*;

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;

// TODO: To remove once 'ProtocolMultiSignature' implements `Serialize`
pub fn key_encode_hex_multisig(from: &ProtocolMultiSignature) -> Result<String, String> {
    Ok(from.to_bytes().encode_hex::<String>())
}

// TODO: To remove once 'ProtocolMultiSignature' implements `Deserialize`
pub fn key_decode_hex_multisig(from: &str) -> Result<ProtocolMultiSignature, String> {
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    ProtocolMultiSignature::from_bytes(&from_vec)
        .map_err(|e| format!("can't decode multi signature: {}", e))
}

// TODO: To remove once 'ProtocolSingleSignature' implements `Serialize`
pub fn key_encode_hex_sig(from: &ProtocolSingleSignature) -> Result<String, String> {
    Ok(from.to_bytes().encode_hex::<String>())
}

// TODO: To remove once 'ProtocolSingleSignature' implements `Deserialize`
pub fn key_decode_hex_sig(from: &str) -> Result<ProtocolSingleSignature, String> {
    let from_vec = Vec::from_hex(from).map_err(|e| format!("can't parse from hex: {}", e))?;
    ProtocolSingleSignature::from_bytes(&from_vec)
        .map_err(|e| format!("can't decode multi signature: {}", e))
}

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
    use super::*;

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    pub fn message() -> Bytes {
        Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap()
    }

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
        let secret_key: ProtocolSignerSecretKey = protocol_initializer.secret_key();
        let verification_key_hex =
            key_encode_hex(verification_key).expect("unexpected hex encoding error");
        let secret_key_hex = key_encode_hex(&secret_key).expect("unexpected hex encoding error");
        let verification_key_restored =
            key_decode_hex(&verification_key_hex).expect("unexpected hex decoding error");
        let secret_key_restored: ProtocolSignerSecretKey =
            key_decode_hex(&secret_key_hex).expect("unexpected hex decoding error");
        assert_eq!(verification_key, verification_key_restored);
        assert_eq!(secret_key.to_bytes(), secret_key_restored.to_bytes());
    }
}
