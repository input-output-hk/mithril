use hex::ToHex;
use slog_scope::{debug, warn};
use std::collections::HashMap;
use thiserror::Error;

// TODO: remove pub
pub use mithril_common::crypto_helper::{
    key_decode_hex, key_encode_hex, Bytes, ProtocolClerk, ProtocolKeyRegistration,
    ProtocolLotteryIndex, ProtocolMultiSignature, ProtocolParameters, ProtocolPartyId,
    ProtocolSignerVerificationKey, ProtocolSingleSignature, ProtocolStake,
    ProtocolStakeDistribution,
};
use mithril_common::entities;

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum ProtocolError {
    #[error("signer already registered")]
    ExistingSigner(),

    #[error("single signature already recorded")]
    ExistingSingleSignature(ProtocolLotteryIndex),

    #[error("codec error: '{0}'")]
    Codec(String),

    #[error("core error: '{0}'")]
    Core(String),

    #[error("no message available")]
    UnavailableMessage(),
}

/// MultiSigner is the cryptographic engine in charge of producing multi signatures from individual signatures
#[cfg_attr(test, automock)]
pub trait MultiSigner: Sync + Send {
    /// Get current message
    fn get_current_message(&self) -> Option<Bytes>;

    /// Update current message
    fn update_current_message(&mut self, message: Bytes) -> Result<(), ProtocolError>;

    /// Get protocol parameters
    fn get_protocol_parameters(&self) -> Option<ProtocolParameters>;

    /// Update protocol parameters
    fn update_protocol_parameters(
        &mut self,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), ProtocolError>;

    /// Get stake distribution
    fn get_stake_distribution(&self) -> ProtocolStakeDistribution;

    /// Update stake distribution
    fn update_stake_distribution(
        &mut self,
        stakes: &ProtocolStakeDistribution,
    ) -> Result<(), ProtocolError>;

    /// Register a signer
    fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), ProtocolError>;

    /// Get signer
    fn get_signer(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, ProtocolError>;

    /// Get signers
    fn get_signers(&self) -> Result<Vec<entities::SignerWithStake>, ProtocolError>;

    /// Registers a single signature
    fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), ProtocolError>;

    /// Retrieves a multi signature from a message
    fn get_multi_signature(
        &self,
        message: &str,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError>;

    /// Create a multi signature from single signatures
    fn create_multi_signature(&mut self) -> Result<Option<ProtocolMultiSignature>, ProtocolError>;
}

/// MultiSignerImpl is an implementation of the MultiSigner
pub struct MultiSignerImpl {
    /// Message that is currently signed
    current_message: Option<Bytes>,

    /// Protocol parameters used for signing
    protocol_parameters: Option<ProtocolParameters>,

    /// Stake distribution used for signing
    stakes: ProtocolStakeDistribution,

    /// Registered signers
    signers: HashMap<ProtocolPartyId, ProtocolSignerVerificationKey>,

    /// Registered single signatures by party and lottery index
    single_signatures:
        HashMap<ProtocolPartyId, HashMap<ProtocolLotteryIndex, ProtocolSingleSignature>>,

    /// Recorded multi signatures by message signed
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
        let mut key_registration = ProtocolKeyRegistration::init(&stakes);
        stakes.iter().for_each(|(party_id, _stake)| {
            if let Some(verification_key) = self.get_signer(*party_id).unwrap() {
                key_registration
                    .register(*party_id, verification_key)
                    .unwrap();
            }
        });
        let closed_registration = key_registration.close();
        ProtocolClerk::from_registration(self.protocol_parameters.unwrap(), closed_registration)
    }
}

impl MultiSigner for MultiSignerImpl {
    /// Get current message
    fn get_current_message(&self) -> Option<Bytes> {
        self.current_message.clone()
    }

    /// Update current message
    fn update_current_message(&mut self, message: Bytes) -> Result<(), ProtocolError> {
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
    ) -> Result<(), ProtocolError> {
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
    ) -> Result<(), ProtocolError> {
        debug!("Update stake distribution to {:?}", stakes);
        self.stakes = stakes.to_owned();
        Ok(())
    }

    /// Get signer verification key
    fn get_signer(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, ProtocolError> {
        Ok(self.signers.get(&party_id).copied())
    }

    /// Get signers
    fn get_signers(&self) -> Result<Vec<entities::SignerWithStake>, ProtocolError> {
        Ok(self
            .get_stake_distribution()
            .iter()
            .filter_map(|(party_id, stake)| match self.get_signer(*party_id) {
                Ok(Some(verification_key)) => match key_encode_hex(verification_key) {
                    Ok(verification_key) => Some(entities::SignerWithStake::new(
                        *party_id as u64,
                        verification_key,
                        *stake as u64,
                    )),
                    Err(_) => None,
                },
                _ => None,
            })
            .collect())
    }

    /// Register a signer
    fn register_signer(
        &mut self,
        party_id: ProtocolPartyId,
        verification_key: &ProtocolSignerVerificationKey,
    ) -> Result<(), ProtocolError> {
        debug!("Register signer {}", party_id);
        match self.signers.insert(party_id, *verification_key) {
            Some(_) => Err(ProtocolError::ExistingSigner()),
            None => Ok(()),
        }
    }

    /// Registers a single signature
    // TODO: Maybe the clerk can be replaced here by a verifier in the crypto library (cf https://github.com/input-output-hk/mithril/issues/162)
    fn register_single_signature(
        &mut self,
        party_id: ProtocolPartyId,
        signature: &ProtocolSingleSignature,
        index: ProtocolLotteryIndex,
    ) -> Result<(), ProtocolError> {
        debug!(
            "Register single signature from {} at index {}",
            party_id, index
        );

        let message = &self
            .get_current_message()
            .ok_or_else(ProtocolError::UnavailableMessage)?;
        let clerk = self.clerk();
        let avk = clerk.compute_avk();
        match signature.verify(&self.protocol_parameters.unwrap(), &avk, message) {
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
                        return Err(ProtocolError::ExistingSingleSignature(index));
                    }
                    None => {
                        signatures.insert(index, signature.to_owned());
                    }
                };

                Ok(())
            }
            Err(e) => Err(ProtocolError::Core(e.to_string())),
        }
    }

    /// Retrieves a multi signature from a message
    fn get_multi_signature(
        &self,
        message: &str,
    ) -> Result<Option<ProtocolMultiSignature>, ProtocolError> {
        debug!("Get multi signature for message {}", message);
        match self.multi_signatures.get(message) {
            Some(multi_signature) => {
                let multi_signature: ProtocolMultiSignature =
                    key_decode_hex(multi_signature).map_err(ProtocolError::Codec)?;
                Ok(Some(multi_signature))
            }
            None => Ok(None),
        }
    }

    /// Create a multi signature from single signatures
    fn create_multi_signature(&mut self) -> Result<Option<ProtocolMultiSignature>, ProtocolError> {
        debug!("Create multi signature");

        let message = &self
            .get_current_message()
            .ok_or_else(ProtocolError::UnavailableMessage)?;
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
                    key_encode_hex(&multi_signature).map_err(ProtocolError::Codec)?,
                );
                self.single_signatures.drain();
                Ok(Some(multi_signature))
            }
            Err(err) => Err(ProtocolError::Core(err.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;

    #[test]
    fn test_multi_signer_current_message_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let current_message_expected = setup_message();
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
            .map(|(party_id, stake, _, _, _)| (*party_id, *stake))
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

        let stake_distribution_expected = setup_signers(5)
            .iter()
            .map(|(party_id, stake, _, _, _)| (*party_id, *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution_expected)
            .expect("update stake distribution failed");

        let signers = setup_signers(5);
        signers
            .iter()
            .for_each(|(party_id, _, verification_key, _, _)| {
                multi_signer
                    .register_signer(*party_id, &verification_key)
                    .expect("register should have succeeded")
            });

        let mut signers_all_expected = Vec::new();
        signers
            .iter()
            .for_each(|(party_id, stake, verification_key_expected, _, _)| {
                let verification_key = multi_signer.get_signer(*party_id);
                assert!(verification_key.as_ref().unwrap().is_some());
                assert_eq!(
                    *verification_key_expected,
                    verification_key.unwrap().unwrap()
                );
                signers_all_expected.push(entities::SignerWithStake::new(
                    *party_id,
                    key_encode_hex(verification_key_expected).unwrap(),
                    *stake,
                ));
            });

        let signers_all = multi_signer
            .get_signers()
            .expect("get signers should have been succeeded");
        assert_eq!(signers_all_expected, signers_all);
    }

    #[test]
    fn test_multi_signer_multi_signature_ok() {
        let mut multi_signer = MultiSignerImpl::new();

        let message = setup_message();
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
            .map(|(party_id, stake, _, _, _)| (*party_id, *stake))
            .collect::<_>();
        multi_signer
            .update_stake_distribution(&stake_distribution)
            .expect("update stake distribution failed");
        signers
            .iter()
            .for_each(|(party_id, _, verification_key, _, _)| {
                multi_signer
                    .register_signer(*party_id, verification_key)
                    .expect("register should have succeeded")
            });

        let mut signatures = Vec::new();
        signers
            .iter()
            .for_each(|(party_id, _, _, protocol_signer, _)| {
                for i in 1..=protocol_parameters.m {
                    if let Some(signature) = protocol_signer.sign(&message, i) {
                        signatures.push((*party_id, signature, i as ProtocolLotteryIndex));
                    }
                }
            });

        let quorum_split = protocol_parameters.k as usize - 1;
        assert!(quorum_split > 1, "quorum should be greater");
        multi_signer
            .create_multi_signature()
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature(&message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_none());
        signatures[0..quorum_split]
            .iter()
            .for_each(|(party_id, signature, index)| {
                multi_signer
                    .register_single_signature(*party_id, signature, *index)
                    .expect("register single signature should not fail");
            });
        multi_signer
            .create_multi_signature()
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature(&message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_none());
        signatures[quorum_split..]
            .iter()
            .for_each(|(party_id, signature, index)| {
                multi_signer
                    .register_single_signature(*party_id, signature, *index)
                    .expect("register single signature should not fail");
            });
        multi_signer
            .create_multi_signature()
            .expect("create multi sgnature should not fail");
        assert!(multi_signer
            .get_multi_signature(&message.encode_hex::<String>())
            .expect("get multi signature should not fail")
            .is_some());
    }
}
