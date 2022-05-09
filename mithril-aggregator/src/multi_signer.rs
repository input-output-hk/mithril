// TODO: to be removed later
#![allow(dead_code)]

use hex::{FromHex, ToHex};
use serde::de::DeserializeOwned;
use serde::Serialize;
use slog_scope::{debug, warn};
use std::collections::HashMap;

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

#[cfg(test)]
use mockall::automock;

/// MultiSigner is the cryptographic engine in charge of producing multi signatures from individual signatures
#[cfg_attr(test, automock)]
pub trait MultiSigner: Sync + Send {
    /// Get current message
    fn get_current_message(&self) -> Option<Bytes>;

    /// Update current message
    fn update_current_message(&mut self, message: Bytes) -> Result<(), String>;

    /// Get protocol parameters
    fn get_protocol_parameters(&self) -> Option<ProtocolParameters>;

    /// Update protocol parameters
    fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), String>;

    /// Get stake distribution
    fn get_stake_distribution(&self) -> ProtocolStakeDistribution;

    /// Update stake distribution
    fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), String>;

    /// Register a signer
    fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), String>;

    /// Get signer
    fn get_signer(&self, party_id: ProtocolPartyId) -> Option<ProtocolSignerVerificationKey>;

    /// Registers a single signature
    fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), String>;

    /// Retrieves a multi signature from a message
    fn get_multi_signature(
        &self,
        message: String,
    ) -> Result<Option<ProtocolMultiSignature>, String>;

    /// Create a multi signature from single signatures
    fn create_multi_signature(&mut self) -> Result<Option<ProtocolMultiSignature>, String>;
}

/// MultiSignerImpl is an implementation of the MultiSigner
pub struct MultiSignerImpl {
    current_message: Option<Bytes>,
    protocol_parameters: Option<ProtocolParameters>,
    stakes: ProtocolStakeDistribution,
    signers: HashMap<ProtocolPartyId, ProtocolSignerVerificationKey>,
    single_signatures:
        HashMap<ProtocolPartyId, HashMap<ProtocolLotteryIndex, ProtocolSingleSignature>>,
    multi_signatures: HashMap<String, String>,
}

impl Default for MultiSignerImpl {
    fn default() -> Self {
        Self::new()
    }
}

impl MultiSignerImpl {
    /// MultiSignerImpl factory
    pub fn new() -> Self {
        debug!("New MultiSignerImpl created");
        Self {
            current_message: None,
            protocol_parameters: None,
            stakes: Vec::new(),
            signers: HashMap::new(),
            single_signatures: HashMap::new(),
            multi_signatures: HashMap::new(),
        }
    }

    /// Creates a clerk
    // TODO: The clerk should be a field of the MultiSignerImpl struct, but this is not possible now as the Clerk uses an unsafe 'Rc'
    pub fn clerk(&self) -> ProtocolClerk {
        let stakes = self.get_stake_distribution();
        let mut key_registration: ProtocolKeyRegistration = KeyReg::new(&stakes);
        stakes.iter().for_each(|(party_id, _stake)| {
            if let Some(verification_key) = self.get_signer(*party_id) {
                key_registration
                    .register(*party_id, verification_key)
                    .unwrap();
            }
        });
        let closed_registration = key_registration.close();
        StmClerk::from_registration(self.protocol_parameters.unwrap(), closed_registration)
    }
}

impl MultiSigner for MultiSignerImpl {
    /// Get current message
    fn get_current_message(&self) -> Option<Bytes> {
        self.current_message.clone()
    }

    /// Update current message
    fn update_current_message(&mut self, message: Bytes) -> Result<(), String> {
        self.current_message = Some(message);
        Ok(())
    }

    /// Get protocol parameters
    fn get_protocol_parameters(&self) -> Option<ProtocolParameters> {
        self.protocol_parameters
    }

    /// Update protocol parameters
    fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), String> {
        debug!("Update protocol parameters to {:?}", protocol_parameters);
        self.protocol_parameters = Some(protocol_parameters.to_owned());
        Ok(())
    }

    /// Get stake distribution
    fn get_stake_distribution(&self) -> ProtocolStakeDistribution {
        self.stakes.clone()
    }

    /// Update stake distribution
    fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), String> {
        debug!("Update stake distribution to {:?}", stakes);
        self.stakes = stakes.to_owned();
        Ok(())
    }

    /// Get signer verification key
    fn get_signer(&self, party_id: ProtocolPartyId) -> Option<ProtocolSignerVerificationKey> {
        self.signers.get(&party_id).copied()
    }

    /// Register a signer
    fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), String> {
        debug!("Register signer {}", party_id);
        self.signers.insert(party_id, *verification_key);
        Ok(())
    }

    /// Registers a single signature
    // TODO: Maybe the clerk can be replaced here by a verifier in the crypto library (cf https://github.com/input-output-hk/mithril/issues/162)
    fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), String> {
        debug!(
            "Register single signature from {} at index {}",
            party_id, index
        );

        let message = &self.get_current_message().unwrap();
        let clerk = self.clerk();
        match clerk.verify_sig(signature, message) {
            Ok(_) => {
                // Register single signature
                self.single_signatures
                    .entry(party_id)
                    .or_insert_with(HashMap::new);
                let signatures = self.single_signatures.get_mut(&party_id).unwrap();
                match signatures.get(&index) {
                    Some(_signature) => {
                        warn!(
                            "Signature already registered from {} at index {}",
                            party_id, index
                        );
                        return Err("signature already registered".to_string());
                    }
                    None => {
                        signatures.insert(index, signature.to_owned());
                    }
                };

                // Create a multi signature
                // TODO: Should be triggered separately
                match self.create_multi_signature() {
                    Ok(Some(multi_signature)) => {
                        debug!(
                            "A multi signature has been created: {}",
                            key_encode_hex_multisig(&multi_signature).unwrap()
                        );
                    }
                    Ok(None) => {
                        warn!("Not ready to create a multi signature: quorum is not reached yet");
                    }
                    Err(e) => {
                        warn!("Error while creating a multi signature: {}", e);
                    }
                }

                Ok(())
            }
            Err(e) => Err(e.to_string()),
        }
    }

    /// Retrieves a multi signature from a message
    fn get_multi_signature(
        &self,
        message: String,
    ) -> Result<Option<ProtocolMultiSignature>, String> {
        debug!("Get multi signature for message {}", message);
        match self.multi_signatures.get(&message) {
            Some(multi_signature) => {
                let multi_signature: ProtocolMultiSignature =
                    key_decode_hex_multisig(multi_signature)
                        .map_err(|e| format!("can't decode multi signature: {}", e))?;
                Ok(Some(multi_signature))
            }
            None => Ok(None),
        }
    }

    /// Create a multi signature from single signatures
    fn create_multi_signature(&mut self) -> Result<Option<ProtocolMultiSignature>, String> {
        debug!("Create multi signature");

        let message = &self.get_current_message().unwrap();
        let signatures: Vec<ProtocolSingleSignature> = self
            .single_signatures
            .iter()
            .flat_map(|(_party_id, h)| {
                h.iter()
                    .map(|(_idx, sig)| sig.to_owned())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        if self.protocol_parameters.unwrap().k > signatures.len() as u64 {
            // Quorum is not reached
            return Ok(None);
        }

        match self.clerk().aggregate(&signatures, message) {
            Ok(multi_signature) => {
                self.multi_signatures.insert(
                    message.encode_hex::<String>(),
                    key_encode_hex_multisig(&multi_signature).unwrap(),
                );
                self.single_signatures.drain();
                Ok(Some(multi_signature))
            }
            Err(e) => Err(e.to_string()),
        }
    }
}

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
mod tests {
    use super::*;

    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    fn message() -> Bytes {
        Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap()
    }

    fn setup_protocol_parameters() -> ProtocolParameters {
        ProtocolParameters {
            m: 10,
            k: 5,
            phi_f: 0.65,
        }
    }

    fn setup_signers(
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

    #[test]
    fn test_key_encode_decode_hex() {
        let protocol_params = setup_protocol_parameters();
        let party_id = 123;
        let stake = 100;
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let protocol_initializer: ProtocolInitializer =
            StmInitializer::setup(protocol_params, party_id, stake, &mut rng);
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

    #[test]
    fn test_multi_signer_current_message_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let current_message_expected = Vec::from_hex("7724e03fb8d84a376a43b8f41518a11c").unwrap();
        multi_signer
            .update_current_message(current_message_expected.clone())
            .expect("update current message failed");

        let current_message = multi_signer
            .get_current_message()
            .expect("current message should have been retrieved");
        assert_eq!(current_message_expected, current_message)
    }

    #[test]
    fn test_multi_signer_protocol_parameters_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let protocol_parameters_expected = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters_expected)
            .expect("update protocol parameters failed");

        let protocol_parameters = multi_signer
            .get_protocol_parameters()
            .expect("protocol parameters should have been retrieved");
        assert_eq!(protocol_parameters_expected, protocol_parameters)
    }

    #[test]
    fn test_multi_signer_stake_distribution_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let stake_distribution_expected = setup_signers(5)
            .iter()
            .map(|(party_id, stake, _, _)| (*party_id, *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .expect("update stake distribution failed");

        let stake_distribution = multi_signer.get_stake_distribution();
        assert_eq!(stake_distribution_expected, stake_distribution)
    }

    #[test]
    fn test_multi_signer_register_signer_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let signers = setup_signers(5);
        signers
            .iter()
            .for_each(|(party_id, _, verification_key, _)| {
                multi_signer
                    .register_signer(*party_id, &verification_key)
                    .expect("register should have succeeded")
            });

        signers
            .iter()
            .for_each(|(party_id, _, verification_key_expected, _)| {
                let verification_key = multi_signer.get_signer(*party_id);
                assert!(verification_key.is_some());
                assert_eq!(*verification_key_expected, verification_key.unwrap());
            });
    }

    #[test]
    fn test_multi_signer_multi_signature_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let message = message();
        multi_signer
            .update_current_message(message.clone())
            .expect("update current message failed");

        let protocol_parameters = setup_protocol_parameters();
        multi_signer
            .update_protocol_parameters(&protocol_parameters)
            .expect("update protocol parameters failed");

        let signers = setup_signers(5);
        let stake_distribution = &signers
            .iter()
            .map(|(party_id, stake, _, _)| (*party_id, *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution)
            .expect("update stake distribution failed");
        signers
            .iter()
            .for_each(|(party_id, _, verification_key, _)| {
                multi_signer
                    .register_signer(*party_id, verification_key)
                    .expect("register should have succeeded")
            });

        let mut signatures = Vec::new();
        signers
            .iter()
            .for_each(|(party_id, _, _, protocol_signer)| {
                for i in 1..=protocol_parameters.m {
                    if let Some(signature) = protocol_signer.sign(&message, i) {
                        signatures.push((*party_id, signature, i as ProtocolLotteryIndex));
                    }
                }
            });

        let quorum_split = protocol_parameters.k as usize - 1;
        assert!(quorum_split > 1, "quorum should be greater");
        assert!(multi_signer
            .get_multi_signature(message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_none());
        signatures[0..quorum_split]
            .iter()
            .for_each(|(party_id, signature, index)| {
                multi_signer
                    .register_single_signature(*party_id, signature, *index)
                    .expect("register single signature should not fail");
            });
        assert!(multi_signer
            .get_multi_signature(message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_none());
        signatures[quorum_split..]
            .iter()
            .for_each(|(party_id, signature, index)| {
                multi_signer
                    .register_single_signature(*party_id, signature, *index)
                    .expect("register single signature should not fail");
            });
        assert!(multi_signer
            .get_multi_signature(message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_some());
    }
}
