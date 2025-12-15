use anyhow::anyhow;
use blake2::digest::Digest;
use digest::FixedOutput;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, Parameters, RegisterError, RegisteredParty, Stake, StmResult,
    signature_scheme::{BlsSigningKey, BlsVerificationKeyProofOfPossession},
};

use super::Signer;

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
    ) -> StmResult<Signer<D>> {
        let mut my_index = None;
        for (i, rp) in closed_reg.reg_parties.iter().enumerate() {
            if rp.0 == self.pk.vk {
                my_index = Some(i as u64);
                break;
            }
        }
        if my_index.is_none() {
            return Err(anyhow!(RegisterError::UnregisteredInitializer));
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
    ) -> StmResult<Signer<D>> {
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
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Initializer> {
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

impl PartialEq for Initializer {
    fn eq(&self, other: &Self) -> bool {
        self.stake == other.stake
            && self.params == other.params
            && self.sk.to_bytes() == other.sk.to_bytes()
            && self.get_verification_key_proof_of_possession()
                == other.get_verification_key_proof_of_possession()
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
            let pk = BlsVerificationKeyProofOfPossession::from(&sk);
            Initializer {
                stake: 1,
                params: Parameters {
                    m: 20973,
                    k: 2422,
                    phi_f: 0.2,
                },
                sk,
                pk,
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
