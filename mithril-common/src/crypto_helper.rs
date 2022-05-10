// TODO: to be removed later
//#![allow(dead_code)]

use hex::{FromHex, ToHex};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use serde::de::DeserializeOwned;
use serde::Serialize;

use mithril::key_reg::KeyReg;
use mithril::msp::{MspPk, MspSk};
use mithril::stm::{
    Index, PartyId, Stake, StmClerk, StmInitializer, StmMultiSig, StmParameters, StmSig, StmSigner,
};

pub type Bytes = Vec<u8>;

// Protocol types alias
type D = blake2::Blake2b;
pub type ProtocolPartyId = PartyId;
pub type ProtocolStake = Stake;
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>;
pub type ProtocolParameters = StmParameters;
pub type ProtocolLotteryIndex = Index;
pub type ProtocolSigner = StmSigner<D>;
pub type ProtocolInitializer = StmInitializer;
pub type ProtocolClerk = StmClerk<D>;
pub type ProtocolKeyRegistration = KeyReg;
pub type ProtocolSingleSignature = StmSig<D>;
pub type ProtocolMultiSignature = StmMultiSig<D>;
pub type ProtocolSignerVerificationKey = MspPk;
pub type ProtocolSignerSecretKey = MspSk;

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

pub mod tests_setup {
    use super::*;

    pub fn setup_message() -> Bytes {
        Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap()
    }

    pub fn setup_protocol_parameters() -> ProtocolParameters {
        ProtocolParameters {
            m: 10,
            k: 5,
            phi_f: 0.65,
        }
    }

    pub fn setup_signers(
        total: u64,
    ) -> Vec<(
        ProtocolPartyId,
        ProtocolStake,
        ProtocolSignerVerificationKey,
        ProtocolSigner,
    )> {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_parameters = setup_protocol_parameters();
        let signers = (0..total)
            .into_iter()
            .map(|party_id| {
                let stake = 1 + rng.next_u64() % 999;
                let protocol_initializer: ProtocolInitializer = StmInitializer::setup(
                    protocol_parameters.clone(),
                    party_id as ProtocolPartyId,
                    stake,
                    &mut rng,
                );
                (
                    party_id as ProtocolPartyId,
                    stake as ProtocolStake,
                    protocol_initializer,
                )
            })
            .collect::<Vec<(ProtocolPartyId, ProtocolStake, ProtocolInitializer)>>();

        let mut key_registration = KeyReg::new(
            &signers
                .iter()
                .map(|(party_id, stake, _)| (*party_id, *stake))
                .collect::<Vec<_>>(),
        );
        signers
            .iter()
            .for_each(|(party_id, _, protocol_initializer)| {
                key_registration
                    .register(*party_id, protocol_initializer.verification_key())
                    .expect("key registration should have succeeded");
            });
        let closed_key_registration = key_registration.close();
        signers
            .into_iter()
            .map(|(party_id, stake, protocol_initializer)| {
                (
                    party_id,
                    stake,
                    protocol_initializer.verification_key(),
                    protocol_initializer.new_signer(closed_key_registration.clone()),
                )
            })
            .collect::<_>()
    }
}

#[cfg(test)]
pub mod tests {
    use super::tests_setup::*;
    use super::*;

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
