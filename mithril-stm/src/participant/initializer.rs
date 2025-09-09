use blake2::digest::Digest;
use digest::FixedOutput;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use crate::bls_multi_signature::{BlsSigningKey, BlsVerificationKeyProofOfPossession};
use crate::key_registration::*;
use crate::{Parameters, RegisterError, Signer, Stake};

/// Wrapper of the MultiSignature Verification key with proof of possession
pub type VerificationKeyProofOfPossession = BlsVerificationKeyProofOfPossession;

/// Initializer for `Signer`.
/// This is the data that is used during the key registration procedure.
/// Once the latter is finished, this instance is consumed into an `Signer`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Initializer {
    /// This participant's stake.
    pub stake: Stake,
    /// Current protocol instantiation parameters.
    pub params: Parameters,
    /// Secret key.
    pub(crate) sk: BlsSigningKey,
    /// Verification (public) key + proof of possession.
    pub(crate) pk: VerificationKeyProofOfPossession,
}

impl Initializer {
    /// Builds an `Initializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, and initialises the structure.
    pub fn new<R: RngCore + CryptoRng>(params: Parameters, stake: Stake, rng: &mut R) -> Self {
        let sk = BlsSigningKey::generate(rng);
        let pk = VerificationKeyProofOfPossession::from(&sk);
        Self {
            stake,
            params,
            sk,
            pk,
        }
    }

    /// Builds an `Initializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, and initialises the structure.
    #[deprecated(since = "0.5.0", note = "Use `new` instead")]
    pub fn setup<R: RngCore + CryptoRng>(params: Parameters, stake: Stake, rng: &mut R) -> Self {
        Self::new(params, stake, rng)
    }

    /// Extract the verification key with proof of possession.
    pub fn get_verification_key_proof_of_possession(&self) -> VerificationKeyProofOfPossession {
        self.pk
    }

    /// Extract the verification key.
    #[deprecated(
        since = "0.5.0",
        note = "Use `get_verification_key_proof_of_possession` instead"
    )]
    pub fn verification_key(&self) -> VerificationKeyProofOfPossession {
        Self::get_verification_key_proof_of_possession(self)
    }

    /// Build the `avk` for the given list of parties.
    ///
    /// Note that if this Initializer was modified *between* the last call to `register`,
    /// then the resulting `Signer` may not be able to produce valid signatures.
    ///
    /// Returns an `Signer` specialized to
    /// * this `Signer`'s ID and current stake
    /// * this `Signer`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    pub fn create_signer<D: Digest + Clone + FixedOutput>(
        self,
        closed_reg: ClosedKeyRegistration<D>,
    ) -> Result<Signer<D>, RegisterError> {
        let mut my_index = None;
        for (i, rp) in closed_reg.reg_parties.iter().enumerate() {
            if rp.0 == self.pk.vk {
                my_index = Some(i as u64);
                break;
            }
        }
        if my_index.is_none() {
            return Err(RegisterError::UnregisteredInitializer);
        }

        Ok(Signer::set_signer(
            my_index.unwrap(),
            self.stake,
            self.params,
            self.sk,
            self.pk.vk,
            closed_reg,
        ))
    }

    /// Build the `avk` for the given list of parties.
    ///
    /// Note that if this Initializer was modified *between* the last call to `register`,
    /// then the resulting `Signer` may not be able to produce valid signatures.
    ///
    /// Returns an `Signer` specialized to
    /// * this `Signer`'s ID and current stake
    /// * this `Signer`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    #[deprecated(since = "0.5.0", note = "Use `create_signer` instead")]
    pub fn new_signer<D: Digest + Clone + FixedOutput>(
        self,
        closed_reg: ClosedKeyRegistration<D>,
    ) -> Result<Signer<D>, RegisterError> {
        Self::create_signer(self, closed_reg)
    }

    /// Creates a new basic signer that does not include closed registration.
    /// Takes `eligible_parties` as a parameter and determines the signer's index in the parties.
    /// `eligible_parties` is verified and trusted which is only run by a full-node
    /// that has already verified the parties.
    pub fn create_basic_signer<D: Digest + Clone + FixedOutput>(
        self,
        eligible_parties: &[RegisteredParty],
    ) -> Option<Signer<D>> {
        let mut parties = eligible_parties.to_vec();
        parties.sort_unstable();
        let mut my_index = None;
        for (i, rp) in parties.iter().enumerate() {
            if rp.0 == self.pk.vk {
                my_index = Some(i as u64);
                break;
            }
        }
        if let Some(index) = my_index {
            Some(Signer::set_basic_signer(
                index,
                self.stake,
                self.params,
                self.sk,
                self.pk.vk,
            ))
        } else {
            None
        }
    }

    /// Creates a new basic signer that does not include closed registration.
    /// Takes `eligible_parties` as a parameter and determines the signer's index in the parties.
    /// `eligible_parties` is verified and trusted which is only run by a full-node
    /// that has already verified the parties.
    #[deprecated(since = "0.5.0", note = "Use `create_basic_signer` instead")]
    pub fn new_core_signer<D: Digest + Clone + FixedOutput>(
        self,
        eligible_parties: &[RegisteredParty],
    ) -> Option<Signer<D>> {
        Self::create_basic_signer(self, eligible_parties)
    }

    /// Convert to bytes
    /// # Layout
    /// * Stake (u64)
    /// * Params
    /// * Secret Key
    /// * Public key (including PoP)
    pub fn to_bytes(&self) -> [u8; 256] {
        let mut out = [0u8; 256];
        out[..8].copy_from_slice(&self.stake.to_be_bytes());
        out[8..32].copy_from_slice(&self.params.to_bytes());
        out[32..64].copy_from_slice(&self.sk.to_bytes());
        out[64..].copy_from_slice(&self.pk.to_bytes());
        out
    }

    /// Convert a slice of bytes to an `Initializer`
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<Initializer, RegisterError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(RegisterError::SerializationError)?);
        let stake = u64::from_be_bytes(u64_bytes);
        let params =
            Parameters::from_bytes(bytes.get(8..32).ok_or(RegisterError::SerializationError)?)?;
        let sk =
            BlsSigningKey::from_bytes(bytes.get(32..).ok_or(RegisterError::SerializationError)?)?;
        let pk = VerificationKeyProofOfPossession::from_bytes(
            bytes.get(64..).ok_or(RegisterError::SerializationError)?,
        )?;

        Ok(Self {
            stake,
            params,
            sk,
            pk,
        })
    }
}
