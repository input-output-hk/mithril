//! API for mithril key certification.
//! Includes the wrappers for StmInitializer and KeyReg, and ProtocolRegistrationErrorWrapper.
//! These wrappers allows keeping mithril-stm agnostic to Cardano, while providing some
//! guarantees that mithril-stm will not be misused in the context of Cardano.  

use crate::crypto_helper::{
    cardano::{OpCert, ParseError, SerDeShelleyFileFormat},
    types::{
        ProtocolParameters, ProtocolPartyId, ProtocolSignerVerificationKeySignature,
        ProtocolStakeDistribution,
    },
    verification_key::ProtocolSignerVerificationKey,
};

use mithril_stm::key_reg::{ClosedKeyReg, KeyReg};
use mithril_stm::stm::{Stake, StmInitializer, StmParameters, StmSigner, StmVerificationKeyPoP};
use mithril_stm::RegisterError;

use crate::crypto_helper::cardano::Sum6KesBytes;
use blake2::{
    digest::{consts::U32, FixedOutput},
    Blake2b, Digest,
};
use kes_summed_ed25519::kes::{Sum6Kes, Sum6KesSig};
use kes_summed_ed25519::traits::{KesSig, KesSk};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

// Protocol types alias
type D = Blake2b<U32>;

/// The KES period that is used to check if the KES keys is expired
pub type KESPeriod = u32;

/// New registration error
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ProtocolRegistrationErrorWrapper {
    /// Error raised when a party id is needed but not provided
    // TODO: Should be removed once the signer certification is fully deployed
    #[error("missing party id")]
    PartyIdMissing,

    /// Error raised when a party id is not available in the Cardano_stake distribution
    #[error("party id does not exist in the stake distribution")]
    PartyIdNonExisting,

    /// Error raised when the operational certificate is missing
    #[error("missing operational certificate")]
    OpCertMissing,

    /// Error raised when an operational certificate is invalid
    #[error("invalid operational certificate")]
    OpCertInvalid,

    /// Error raised when a KES Signature verification fails
    #[error("KES signature verification error: CurrentKesPeriod={0}, StartKesPeriod={1}")]
    KesSignatureInvalid(u32, u64),

    /// Error raised when a KES Signature is needed but not provided
    #[error("missing KES signature")]
    KesSignatureMissing,

    /// Error raised when a KES Period is needed but not provided
    #[error("missing KES period")]
    KesPeriodMissing,

    /// Error raised when a pool address encoding fails
    #[error("pool address encoding error")]
    PoolAddressEncoding,

    /// Error raised when a core registration error occurs
    #[error("core registration error: '{0}'")]
    CoreRegister(#[from] RegisterError),
}

/// New initializer error
#[derive(Error, Debug)]
pub enum ProtocolInitializerErrorWrapper {
    /// Error raised when a codec parse error occurs
    #[error("codec parse error: '{0}'")]
    Codec(#[from] ParseError),

    /// Error raised when a KES update error occurs
    #[error("KES key cannot be updated for period {0}")]
    KesUpdate(KESPeriod),

    /// Period of key file does not match with period provided by user
    #[error("Period of key file, {0}, does not match with period provided by user, {1}")]
    KesMismatch(KESPeriod, KESPeriod),
}
/// Wrapper structure for [MithrilStm:StmInitializer](mithril_stm::stm::StmInitializer).
/// It now obtains a KES signature over the Mithril key. This allows the signers prove
/// their correct identity with respect to a Cardano PoolID.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializerWrapper {
    stm_initializer: StmInitializer,
    kes_signature: Option<ProtocolSignerVerificationKeySignature>, // todo: The option is ONLY for a smooth transition. We have to remove this.
}

/// Wrapper structure for [MithrilStm:KeyReg](mithril_stm::key_reg::KeyReg).
/// The wrapper not only contains a map between `Mithril vkey <-> Stake`, but also
/// a map `PoolID <-> Stake`. This information is recovered from the node state, and
/// is used to verify the identity of a Mithril signer. Furthermore, the `register` function
/// of the wrapper forces the registrar to check that the KES signature over the Mithril key
/// is valid with respect to the PoolID.
#[derive(Debug, Clone)]
pub struct KeyRegWrapper {
    stm_key_reg: KeyReg,
    stake_distribution: HashMap<ProtocolPartyId, Stake>,
}

impl StmInitializerWrapper {
    /// Builds an `StmInitializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, signs the verification
    /// key with a provided KES signing key, and initializes the structure.
    pub fn setup<R: RngCore + CryptoRng, P: AsRef<Path>>(
        params: StmParameters,
        kes_sk_path: Option<P>,
        kes_period: Option<KESPeriod>,
        stake: Stake,
        rng: &mut R,
    ) -> Result<Self, ProtocolInitializerErrorWrapper> {
        let stm_initializer = StmInitializer::setup(params, stake, rng);
        let kes_signature = if let Some(kes_sk_path) = kes_sk_path {
            let mut kes_sk_bytes = Sum6KesBytes::from_file(kes_sk_path)?;
            let mut kes_sk = Sum6Kes::try_from(&mut kes_sk_bytes)
                .map_err(ProtocolInitializerErrorWrapper::Codec)?;
            let kes_sk_period = kes_sk.get_period();
            let provided_period = kes_period.unwrap_or_default();
            if kes_sk_period > provided_period {
                return Err(ProtocolInitializerErrorWrapper::KesMismatch(
                    kes_sk_period,
                    provided_period,
                ));
            }

            // We need to perform the evolutions
            for period in kes_sk_period..provided_period {
                kes_sk
                    .update()
                    .map_err(|_| ProtocolInitializerErrorWrapper::KesUpdate(period))?;
            }

            Some(kes_sk.sign(&stm_initializer.verification_key().to_bytes()))
        } else {
            println!("WARNING: Non certified signer registration by providing only a Pool Id is decommissionned and must be used for tests only!");
            None
        };

        Ok(Self {
            stm_initializer,
            kes_signature,
        })
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKeyPoP {
        self.stm_initializer.verification_key()
    }

    /// Extract the verification key signature.
    pub fn verification_key_signature(&self) -> Option<ProtocolSignerVerificationKeySignature> {
        self.kes_signature
    }

    /// Extract the protocol parameters of the initializer
    pub fn get_protocol_parameters(&self) -> ProtocolParameters {
        self.stm_initializer.params
    }

    /// Extract the stake of the party
    pub fn get_stake(&self) -> Stake {
        self.stm_initializer.stake
    }

    /// Build the `avk` for the given list of parties.
    ///
    /// Note that if this StmInitializer was modified *between* the last call to `register`,
    /// then the resulting `StmSigner` may not be able to produce valid signatures.
    ///
    /// Returns a `StmSignerWrapper` specialized to
    /// * this `StmSignerWrapper`'s ID and current stake
    /// * this `StmSignerWrapper`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    pub fn new_signer(
        self,
        closed_reg: ClosedKeyReg<D>,
    ) -> Result<StmSigner<D>, ProtocolRegistrationErrorWrapper> {
        Ok(self.stm_initializer.new_signer(closed_reg)?)
    }

    /// Convert to bytes
    /// # Layout
    /// * StmInitialiser
    /// * KesSignature
    pub fn to_bytes(&self) -> [u8; 704] {
        let mut out = [0u8; 704];
        out[..256].copy_from_slice(&self.stm_initializer.to_bytes());
        // out[256..].copy_from_slice(&self.kes_signature.to_bytes()); todo: repair
        out
    }

    /// Convert a slice of bytes to an `StmInitializerWrapper`
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
        let stm_initializer = StmInitializer::from_bytes(bytes)?;
        let kes_signature =
            Sum6KesSig::from_bytes(&bytes[256..]).map_err(|_| RegisterError::SerializationError)?;

        Ok(Self {
            stm_initializer,
            kes_signature: Some(kes_signature),
        })
    }
}

impl KeyRegWrapper {
    /// New Initialisation function. We temporarily keep the other init function,
    /// but we should eventually transition to only use this one.
    pub fn init(stake_dist: &ProtocolStakeDistribution) -> Self {
        Self {
            stm_key_reg: KeyReg::init(),
            stake_distribution: HashMap::from_iter(stake_dist.to_vec()),
        }
    }

    /// Register a new party. For a successful registration, the registrar needs to
    /// provide the OpCert (in cbor form), the cold VK, a KES signature, and a
    /// Mithril key (with its corresponding Proof of Possession).
    pub fn register(
        &mut self,
        party_id: Option<ProtocolPartyId>, // TODO: Parameter should be removed once the signer certification is fully deployed
        opcert: Option<OpCert>, // TODO: Option should be removed once the signer certification is fully deployed
        kes_sig: Option<ProtocolSignerVerificationKeySignature>, // TODO: Option should be removed once the signer certification is fully deployed
        kes_period: Option<KESPeriod>,
        pk: ProtocolSignerVerificationKey,
    ) -> Result<ProtocolPartyId, ProtocolRegistrationErrorWrapper> {
        let pool_id_bech32: ProtocolPartyId = if let Some(opcert) = opcert {
            opcert
                .validate()
                .map_err(|_| ProtocolRegistrationErrorWrapper::OpCertInvalid)?;
            let mut pool_id = None;
            let sig = kes_sig.ok_or(ProtocolRegistrationErrorWrapper::KesSignatureMissing)?;
            let kes_period =
                kes_period.ok_or(ProtocolRegistrationErrorWrapper::KesPeriodMissing)?;
            let kes_period_try_min = std::cmp::max(0, kes_period.saturating_sub(1));
            let kes_period_try_max = std::cmp::min(64, kes_period.saturating_add(1));
            for kes_period_try in kes_period_try_min..kes_period_try_max {
                if sig
                    .verify(kes_period_try, &opcert.kes_vk, &pk.to_bytes())
                    .is_ok()
                {
                    pool_id = Some(
                        opcert
                            .compute_protocol_party_id()
                            .map_err(|_| ProtocolRegistrationErrorWrapper::PoolAddressEncoding)?,
                    );
                    break;
                }
            }
            pool_id.ok_or(ProtocolRegistrationErrorWrapper::KesSignatureInvalid(
                kes_period,
                opcert.start_kes_period,
            ))?
        } else {
            if cfg!(not(feature = "allow_skip_signer_certification")) {
                Err(ProtocolRegistrationErrorWrapper::OpCertMissing)?
            }
            party_id.ok_or(ProtocolRegistrationErrorWrapper::PartyIdMissing)?
        };

        if let Some(&stake) = self.stake_distribution.get(&pool_id_bech32) {
            self.stm_key_reg
                .register(stake, pk.into())
                .map_err(ProtocolRegistrationErrorWrapper::CoreRegister)?;
            return Ok(pool_id_bech32);
        }
        Err(ProtocolRegistrationErrorWrapper::PartyIdNonExisting)
    }

    /// Finalize the key registration.
    /// This function disables `KeyReg::register`, consumes the instance of `self`, and returns a `ClosedKeyReg`.
    pub fn close<D: Digest + FixedOutput>(self) -> ClosedKeyReg<D> {
        self.stm_key_reg.close()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::crypto_helper::cardano::ColdKeyGenerator;

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;
    use std::{fs, path::PathBuf};

    fn setup_temp_directory() -> PathBuf {
        let temp_dir = std::env::temp_dir().join("mithril_cardano_key_certification");
        fs::create_dir_all(&temp_dir).expect("temp dir creation should not fail");
        temp_dir
    }

    fn create_cryptographic_material(party_idx: u64) -> (ProtocolPartyId, PathBuf, PathBuf) {
        let temp_dir = setup_temp_directory();
        let keypair = ColdKeyGenerator::create_deterministic_keypair([party_idx as u8; 32]);
        let mut dummy_buffer = [0u8; Sum6Kes::SIZE + 4];
        let mut dummy_seed = [party_idx as u8; 32];
        let (kes_secret_key, kes_verification_key) =
            Sum6Kes::keygen(&mut dummy_buffer, &mut dummy_seed);
        let mut kes_bytes = Sum6KesBytes([0u8; Sum6Kes::SIZE + 4]);
        kes_bytes.0.copy_from_slice(&kes_secret_key.clone_sk());
        let operational_certificate = OpCert::new(kes_verification_key, 0, 0, keypair);
        let kes_secret_key_file = temp_dir.join(format!("kes{party_idx}.skey"));
        kes_bytes
            .to_file(&kes_secret_key_file)
            .expect("KES secret key file export should not fail");
        let operational_certificate_file = temp_dir.join(format!("pool{party_idx}.cert"));
        operational_certificate
            .to_file(&operational_certificate_file)
            .expect("operational certificate file export should not fail");
        let party_id = operational_certificate
            .compute_protocol_party_id()
            .expect("compute protocol party id should not fail");
        (party_id, operational_certificate_file, kes_secret_key_file)
    }

    #[test]
    fn test_vector_key_reg() {
        let params = StmParameters {
            m: 5,
            k: 5,
            phi_f: 1.0,
        };
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let (party_id_1, operational_certificate_file_1, kes_secret_key_file_1) =
            create_cryptographic_material(1);
        let (party_id_2, operational_certificate_file_2, kes_secret_key_file_2) =
            create_cryptographic_material(2);

        let mut key_reg = KeyRegWrapper::init(&vec![(party_id_1, 10), (party_id_2, 3)]);

        let initializer_1 = StmInitializerWrapper::setup(
            params,
            Some(kes_secret_key_file_1),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert1: OpCert = OpCert::from_file(operational_certificate_file_1)
            .expect("opcert deserialization should not fail");

        let key_registration_1 = key_reg.register(
            None,
            Some(opcert1),
            initializer_1.kes_signature,
            Some(0),
            initializer_1.stm_initializer.verification_key().into(),
        );
        assert!(key_registration_1.is_ok());

        let initializer_2 = StmInitializerWrapper::setup(
            params,
            Some(kes_secret_key_file_2),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert2: OpCert = OpCert::from_file(operational_certificate_file_2)
            .expect("opcert deserialization should not fail");

        let key_registration_2 = key_reg.register(
            None,
            Some(opcert2),
            initializer_2.kes_signature,
            Some(0),
            initializer_2.stm_initializer.verification_key().into(),
        );
        assert!(key_registration_2.is_ok())
    }
}
