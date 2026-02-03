use anyhow::anyhow;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, RegisterError, RegistrationEntry,
    RegistrationEntryForConcatenation, Stake, StmResult,
    VerificationKeyProofOfPossessionForConcatenation, proof_system::ConcatenationProofSigner,
    signature_scheme::BlsSigningKey,
};
#[cfg(feature = "future_snark")]
use crate::{VerificationKeyForSnark, signature_scheme::SchnorrSigningKey};

use super::Signer;

/// Structure responsible of creating a signer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Initializer {
    /// Stake of the participant
    pub stake: Stake,
    /// Protocol parameters.
    #[serde(rename = "params")]
    pub parameters: Parameters,
    /// Signing key for concatenation proof system.
    #[serde(rename = "sk")]
    pub bls_signing_key: BlsSigningKey,
    /// Verification key for concatenation proof system.
    #[serde(rename = "pk")]
    pub bls_verification_key_proof_of_possession: VerificationKeyProofOfPossessionForConcatenation,
    /// Signing key for snark proof system.
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schnorr_signing_key: Option<SchnorrSigningKey>,
    /// Verification key for snark proof system.
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schnorr_verification_key: Option<VerificationKeyForSnark>,
}

impl Initializer {
    /// Creates a new initializer
    pub fn new<R: RngCore + CryptoRng>(parameters: Parameters, stake: Stake, rng: &mut R) -> Self {
        let bls_signing_key = BlsSigningKey::generate(rng);
        let bls_verification_key_proof_of_possession =
            VerificationKeyProofOfPossessionForConcatenation::from(&bls_signing_key);
        #[cfg(feature = "future_snark")]
        let (schnorr_signing_key, schnorr_verification_key) = {
            let sk = SchnorrSigningKey::generate(rng);
            let vk = VerificationKeyForSnark::new_from_signing_key(sk.clone())
                .expect("verification key creation from valid signing key should not fail");
            (Some(sk), Some(vk))
        };
        Self {
            stake,
            parameters,
            bls_signing_key,
            bls_verification_key_proof_of_possession,
            #[cfg(feature = "future_snark")]
            schnorr_signing_key,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
        }
    }

    /// Attempts to generate a new signer from the current registration state.
    ///
    /// # Process
    /// 1. Verifies that registration is closed (determined by total stake threshold)
    /// 2. Confirms the initializer is registered and retrieves its signer index
    /// 3. Constructs the Merkle tree commitment
    /// 4. Creates the underlying proof system signer (currently only the concatenation proof system)
    ///
    /// # Errors
    /// Returns an error if:
    /// - Registration is not yet closed
    /// - The initializer is not registered
    pub fn try_create_signer<D: MembershipDigest>(
        self,
        closed_key_registration: &ClosedKeyRegistration,
    ) -> StmResult<Signer<D>> {
        let registration_entry = RegistrationEntry::new(
            self.bls_verification_key_proof_of_possession,
            self.stake,
            #[cfg(feature = "future_snark")]
            self.schnorr_verification_key,
        )?;

        let signer_index = closed_key_registration
            .get_signer_index_for_registration(
                &(registration_entry, closed_key_registration.total_stake).into(),
            )
            .ok_or_else(|| anyhow!(RegisterError::UnregisteredInitializer))?;

        let key_registration_commitment = closed_key_registration
            .to_merkle_tree::<D::ConcatenationHash, RegistrationEntryForConcatenation>();

        // Create concatenation proof signer
        let concatenation_proof_signer = ConcatenationProofSigner::new(
            registration_entry.get_stake(),
            closed_key_registration.total_stake,
            self.parameters,
            self.bls_signing_key,
            self.bls_verification_key_proof_of_possession.vk,
            key_registration_commitment,
        );

        // Create and return signer
        Ok(Signer::new(
            signer_index,
            concatenation_proof_signer,
            closed_key_registration.clone(),
            self.parameters,
            registration_entry.get_stake(),
        ))
    }

    /// Extract the verification key with proof of possession.
    pub fn get_verification_key_proof_of_possession_for_concatenation(
        &self,
    ) -> VerificationKeyProofOfPossessionForConcatenation {
        self.bls_verification_key_proof_of_possession
    }

    /// Convert to bytes
    /// # Layout
    /// * Stake (u64)
    /// * Params
    /// * BLS signing key
    /// * BLS verification key (including PoP)
    /// * [Future Snark - Schnorr Signing Key]
    /// * [Future Snark - Schnorr Verification Key]
    pub fn to_bytes(&self) -> Vec<u8> {
        #[cfg(feature = "future_snark")]
        let mut out = [0u8; 352];
        #[cfg(not(feature = "future_snark"))]
        let mut out = [0u8; 256];

        out[..8].copy_from_slice(&self.stake.to_be_bytes());
        out[8..32].copy_from_slice(&self.parameters.to_bytes());
        out[32..64].copy_from_slice(&self.bls_signing_key.to_bytes());
        out[64..256].copy_from_slice(&self.bls_verification_key_proof_of_possession.to_bytes());

        #[cfg(feature = "future_snark")]
        if let Some(schnorr_sk) = &self.schnorr_signing_key {
            out[256..288].copy_from_slice(&schnorr_sk.to_bytes());
        }
        #[cfg(feature = "future_snark")]
        if let Some(schnorr_vk) = &self.schnorr_verification_key {
            out[288..352].copy_from_slice(&schnorr_vk.to_bytes());
        }

        out.to_vec()
    }

    /// Convert a slice of bytes to an `Initializer`
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Initializer> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(RegisterError::SerializationError)?);
        let stake = u64::from_be_bytes(u64_bytes);
        let params =
            Parameters::from_bytes(bytes.get(8..32).ok_or(RegisterError::SerializationError)?)?;
        let bls_signing_key =
            BlsSigningKey::from_bytes(bytes.get(32..64).ok_or(RegisterError::SerializationError)?)?;
        let bls_verification_key_proof_of_possession =
            VerificationKeyProofOfPossessionForConcatenation::from_bytes(
                bytes.get(64..256).ok_or(RegisterError::SerializationError)?,
            )?;

        #[cfg(feature = "future_snark")]
        let (schnorr_signing_key, schnorr_verification_key) = {
            let sk = SchnorrSigningKey::from_bytes(
                bytes.get(256..288).ok_or(RegisterError::SerializationError)?,
            )?;
            let vk = VerificationKeyForSnark::from_bytes(
                bytes.get(288..352).ok_or(RegisterError::SerializationError)?,
            )?;
            (Some(sk), Some(vk))
        };

        Ok(Self {
            stake,
            parameters: params,
            bls_signing_key,
            bls_verification_key_proof_of_possession,
            #[cfg(feature = "future_snark")]
            schnorr_signing_key,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
        })
    }
}

impl PartialEq for Initializer {
    fn eq(&self, other: &Self) -> bool {
        let base_eq = self.stake == other.stake
            && self.parameters == other.parameters
            && self.bls_signing_key.to_bytes() == other.bls_signing_key.to_bytes()
            && self.get_verification_key_proof_of_possession_for_concatenation()
                == other.get_verification_key_proof_of_possession_for_concatenation();

        #[cfg(feature = "future_snark")]
        let base_eq = base_eq
            && self.schnorr_signing_key == other.schnorr_signing_key
            && self.schnorr_verification_key == other.schnorr_verification_key;

        base_eq
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod golden {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::*;

        const GOLDEN_JSON: &str = r#"
            {
                "stake":1,
                "params":
                {
                    "m":20973,
                    "k":2422,
                    "phi_f":0.2
                },
                "sk":[64,129,87,121,27,239,221,215,2,103,45,207,207,201,157,163,81,47,156,14,168,24,137,15,203,106,183,73,88,14,242,207],
                "pk":
                {
                    "vk":[143,161,255,48,78,57,204,220,25,221,164,252,248,14,56,126,186,135,228,188,145,181,52,200,97,99,213,46,0,199,193,89,187,88,29,135,173,244,86,36,83,54,67,164,6,137,94,72,6,105,128,128,93,48,176,11,4,246,138,48,180,133,90,142,192,24,193,111,142,31,76,111,110,234,153,90,208,192,31,124,95,102,49,158,99,52,220,165,94,251,68,69,121,16,224,194],
                    "pop":[168,50,233,193,15,136,65,72,123,148,129,176,38,198,209,47,28,204,176,144,57,251,42,28,66,76,89,97,158,63,54,198,194,176,135,221,14,185,197,225,202,98,243,74,233,225,143,151,147,177,170,117,66,165,66,62,33,216,232,75,68,114,195,22,100,65,44,198,4,166,102,233,253,240,59,175,60,117,142,114,140,122,17,87,110,187,1,17,10,195,154,13,249,86,54,226]
                }
            }
        "#;

        fn golden_value() -> Initializer {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = BlsSigningKey::generate(&mut rng);
            let pk = VerificationKeyProofOfPossessionForConcatenation::from(&sk);
            Initializer {
                stake: 1,
                parameters: Parameters {
                    m: 20973,
                    k: 2422,
                    phi_f: 0.2,
                },
                bls_signing_key: sk,
                bls_verification_key_proof_of_possession: pk,
                #[cfg(feature = "future_snark")]
                schnorr_signing_key: None,
                #[cfg(feature = "future_snark")]
                schnorr_verification_key: None,
            }
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
