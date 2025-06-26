//! API for mithril key certification.
//! Includes the wrappers for StmInitializer and KeyReg, and ProtocolRegistrationErrorWrapper.
//! These wrappers allows keeping mithril-stm agnostic to Cardano, while providing some
//! guarantees that mithril-stm will not be misused in the context of Cardano.  

use std::{collections::HashMap, sync::Arc};

use blake2::{
    digest::{consts::U32, FixedOutput},
    Blake2b, Digest,
};
use kes_summed_ed25519::kes::Sum6KesSig;
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use mithril_stm::{
    ClosedKeyReg, KeyReg, RegisterError, Stake, StmInitializer, StmParameters, StmSigner,
    StmVerificationKeyPoP,
};

use crate::{
    crypto_helper::{
        cardano::{KesSigner, KesVerifier, KesVerifierStandard},
        types::{
            ProtocolParameters, ProtocolPartyId, ProtocolSignerVerificationKey,
            ProtocolSignerVerificationKeySignature, ProtocolStakeDistribution,
        },
        ProtocolOpCert,
    },
    StdError, StdResult,
};

// Protocol types alias
type D = Blake2b<U32>;

/// The KES period that is used to check if the KES keys is expired
pub type KesPeriod = u32;

/// New registration error
#[derive(Error, Debug)]
pub enum ProtocolRegistrationErrorWrapper {
    /// Error raised when a party id is needed but not provided
    ///
    /// Used only for testing when SPO pool id is not certified
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
    #[error("core registration error")]
    CoreRegister(#[source] RegisterError),
}

/// New initializer error
#[derive(Error, Debug)]
pub enum ProtocolInitializerErrorWrapper {
    /// Error raised when the underlying protocol initializer fails
    #[error("protocol initializer error")]
    ProtocolInitializer(#[source] StdError),

    /// Error raised when a KES update error occurs
    #[error("KES key cannot be updated for period {0}")]
    KesUpdate(KesPeriod),

    /// Period of key file does not match with period provided by user
    #[error("Period of key file, {0}, does not match with period provided by user, {1}")]
    KesMismatch(KesPeriod, KesPeriod),
}

/// Wrapper structure for [MithrilStm:StmInitializer](mithril_stm::stm::StmInitializer).
/// It now obtains a KES signature over the Mithril key. This allows the signers prove
/// their correct identity with respect to a Cardano PoolID.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializerWrapper {
    /// The StmInitializer
    stm_initializer: StmInitializer,

    /// The KES signature over the Mithril key
    ///
    /// None is used only for testing when SPO pool id is not certified
    kes_signature: Option<Sum6KesSig>,
}

impl StmInitializerWrapper {
    /// Builds an `StmInitializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, signs the verification
    /// key with a provided KES signer implementation, and initializes the structure.
    pub fn setup<R: RngCore + CryptoRng>(
        params: StmParameters,
        kes_signer: Option<Arc<dyn KesSigner>>,
        kes_period: Option<KesPeriod>,
        stake: Stake,
        rng: &mut R,
    ) -> StdResult<Self> {
        let stm_initializer = StmInitializer::setup(params, stake, rng);
        let kes_signature = if let Some(kes_signer) = kes_signer {
            let (signature, _op_cert) = kes_signer.sign(
                &stm_initializer.verification_key().to_bytes(),
                kes_period.unwrap_or_default(),
            )?;

            Some(signature)
        } else {
            println!("WARNING: Non certified signer registration by providing only a Pool Id is decommissioned and must be used for tests only!");
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
        self.kes_signature.map(|k| k.into())
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
        self.stm_initializer
            .new_signer(closed_reg)
            .map_err(ProtocolRegistrationErrorWrapper::CoreRegister)
    }

    /// Convert to bytes
    /// # Layout
    /// * StmInitialiser
    /// * KesSignature
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&self.stm_initializer.to_bytes());
        if let Some(kes_signature) = &self.kes_signature {
            out.extend_from_slice(&kes_signature.to_bytes());
        }

        out
    }

    /// Convert a slice of bytes to an `StmInitializerWrapper`
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
        let stm_initializer =
            StmInitializer::from_bytes(bytes.get(..256).ok_or(RegisterError::SerializationError)?)?;
        let bytes = bytes.get(256..).ok_or(RegisterError::SerializationError)?;
        let kes_signature = if bytes.is_empty() {
            None
        } else {
            Some(Sum6KesSig::from_bytes(bytes).map_err(|_| RegisterError::SerializationError)?)
        };

        Ok(Self {
            stm_initializer,
            kes_signature,
        })
    }

    cfg_test_tools! {
        /// Override the protocol parameters of the `StmInitializer` for testing purposes only.
        pub fn override_protocol_parameters(&mut self, protocol_parameters: &ProtocolParameters) {
            self.stm_initializer.params = protocol_parameters.to_owned();
        }
    }
}

/// Wrapper structure for [MithrilStm:KeyReg](mithril_stm::key_reg::KeyReg).
/// The wrapper not only contains a map between `Mithril vkey <-> Stake`, but also
/// a map `PoolID <-> Stake`. This information is recovered from the node state, and
/// is used to verify the identity of a Mithril signer. Furthermore, the `register` function
/// of the wrapper forces the registrar to check that the KES signature over the Mithril key
/// is valid with respect to the PoolID.
#[derive(Debug, Clone)]
pub struct KeyRegWrapper {
    kes_verifier: Arc<dyn KesVerifier>,
    stm_key_reg: KeyReg,
    stake_distribution: HashMap<ProtocolPartyId, Stake>,
}

impl KeyRegWrapper {
    /// New Initialisation function. We temporarily keep the other init function,
    /// but we should eventually transition to only use this one.
    pub fn init(stake_dist: &ProtocolStakeDistribution) -> Self {
        Self {
            kes_verifier: Arc::new(KesVerifierStandard),
            stm_key_reg: KeyReg::init(),
            stake_distribution: HashMap::from_iter(stake_dist.to_vec()),
        }
    }

    /// Register a new party. For a successful registration, the registrar needs to
    /// provide the OpCert (in cbor form), the cold VK, a KES signature, and a
    /// Mithril key (with its corresponding Proof of Possession).
    pub fn register(
        &mut self,
        party_id: Option<ProtocolPartyId>, // Used for only for testing when SPO pool id is not certified
        opcert: Option<ProtocolOpCert>, // Used for only for testing when SPO pool id is not certified
        kes_sig: Option<ProtocolSignerVerificationKeySignature>, // Used for only for testing when SPO pool id is not certified
        kes_period: Option<KesPeriod>,
        pk: ProtocolSignerVerificationKey,
    ) -> Result<ProtocolPartyId, ProtocolRegistrationErrorWrapper> {
        let pool_id_bech32: ProtocolPartyId = if let Some(opcert) = opcert {
            let signature = kes_sig.ok_or(ProtocolRegistrationErrorWrapper::KesSignatureMissing)?;
            let kes_period =
                kes_period.ok_or(ProtocolRegistrationErrorWrapper::KesPeriodMissing)?;
            if self
                .kes_verifier
                .verify(&pk.to_bytes(), &signature, &opcert, kes_period)
                .is_ok()
            {
                opcert
                    .compute_protocol_party_id()
                    .map_err(|_| ProtocolRegistrationErrorWrapper::PoolAddressEncoding)?
            } else {
                return Err(ProtocolRegistrationErrorWrapper::KesSignatureInvalid(
                    kes_period,
                    opcert.start_kes_period,
                ));
            }
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
    use crate::crypto_helper::cardano::tests_setup::KesCryptographicMaterialForTest;
    use crate::crypto_helper::cardano::{
        tests_setup::create_kes_cryptographic_material, KesSignerStandard,
    };
    use crate::crypto_helper::{OpCert, SerDeShelleyFileFormat};

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn test_vector_key_reg() {
        let params = StmParameters {
            m: 5,
            k: 5,
            phi_f: 1.0,
        };
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let KesCryptographicMaterialForTest {
            party_id: party_id_1,
            operational_certificate_file: operational_certificate_file_1,
            kes_secret_key_file: kes_secret_key_file_1,
        } = create_kes_cryptographic_material(1, 0 as KesPeriod, "test_vector_key_reg");
        let KesCryptographicMaterialForTest {
            party_id: party_id_2,
            operational_certificate_file: operational_certificate_file_2,
            kes_secret_key_file: kes_secret_key_file_2,
        } = create_kes_cryptographic_material(2, 0 as KesPeriod, "test_vector_key_reg");

        let mut key_reg = KeyRegWrapper::init(&vec![(party_id_1, 10), (party_id_2, 3)]);

        let initializer_1 = StmInitializerWrapper::setup(
            params,
            Some(Arc::new(KesSignerStandard::new(
                kes_secret_key_file_1,
                operational_certificate_file_1.clone(),
            ))),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert1 = OpCert::from_file(operational_certificate_file_1)
            .expect("opcert deserialization should not fail")
            .into();

        let key_registration_1 = key_reg.register(
            None,
            Some(opcert1),
            initializer_1.verification_key_signature(),
            Some(0),
            initializer_1.stm_initializer.verification_key().into(),
        );
        assert!(key_registration_1.is_ok());

        let initializer_2 = StmInitializerWrapper::setup(
            params,
            Some(Arc::new(KesSignerStandard::new(
                kes_secret_key_file_2,
                operational_certificate_file_2.clone(),
            ))),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert2 = OpCert::from_file(operational_certificate_file_2)
            .expect("opcert deserialization should not fail")
            .into();

        let key_registration_2 = key_reg.register(
            None,
            Some(opcert2),
            initializer_2.verification_key_signature(),
            Some(0),
            initializer_2.stm_initializer.verification_key().into(),
        );
        assert!(key_registration_2.is_ok())
    }

    const GOLDEN_STM_INITIALIZER_WRAPPER_JSON: &str = r#"
    {
        "stm_initializer": {
            "stake": 9497432569,
            "params": {
                "m": 20973,
                "k": 2422,
                "phi_f": 0.2
            },
            "sk": [49, 181, 118, 110, 190, 161, 107, 218, 165, 20, 147, 129, 193, 79, 160, 0, 37, 23, 102, 223, 88, 174, 208, 70, 97, 79, 174, 51, 28, 0, 192, 210],
            "pk": {
                "vk": [173, 149, 133, 21, 100, 254, 36, 74, 165, 174, 56, 9, 145, 190, 48, 14, 12, 193, 243, 3, 200, 148, 221, 124, 170, 143, 89, 5, 168, 0, 226, 125, 61, 181, 190, 80, 62, 199, 99, 161, 117, 49, 65, 34, 81, 96, 34, 81, 2, 235, 173, 57, 58, 128, 49, 22, 242, 42, 30, 137, 6, 51, 77, 57, 142, 192, 140, 161, 206, 206, 213, 114, 156, 191, 127, 167, 167, 9, 39, 29, 97, 166, 134, 76, 55, 179, 72, 29, 41, 251, 14, 71, 89, 181, 31, 115],
                "pop": [171, 0, 214, 91, 37, 208, 228, 71, 228, 31, 138, 0, 237, 175, 24, 45, 160, 117, 14, 210, 23, 46, 235, 83, 45, 9, 58, 207, 18, 36, 31, 160, 252, 111, 69, 102, 248, 205, 46, 71, 24, 38, 41, 77, 29, 129, 95, 16, 136, 114, 250, 44, 230, 184, 222, 122, 120, 58, 249, 103, 48, 121, 141, 244, 243, 26, 252, 60, 230, 64, 75, 3, 86, 107, 198, 198, 117, 242, 107, 104, 219, 209, 211, 255, 174, 203, 43, 141, 34, 146, 25, 181, 212, 38, 194, 99]
            }
        },
        "kes_signature": {
            "sigma": {
                "sigma": {
                    "sigma": {
                        "sigma": {
                            "sigma": {
                                "sigma": [71, 225, 146, 98, 81, 62, 28, 21, 7, 157, 88, 4, 226, 126, 27, 133, 146, 171, 216, 170, 77, 17, 38, 146, 98, 202, 35, 87, 166, 162, 25, 207, 105, 174, 48, 225, 152, 68, 19, 109, 72, 241, 69, 111, 22, 214, 72, 20, 81, 56, 181, 104, 69, 121, 173, 194, 37, 60, 16, 155, 86, 99, 253, 7],
                                "lhs_pk": [
                                    91, 82, 235, 39, 167, 29, 141, 253, 163, 163, 55, 185, 162, 191, 52, 8, 245, 7, 104, 22, 182, 239, 133, 138, 131, 15, 233, 116, 147, 251, 182, 140],
                                    "rhs_pk": [189, 26, 9, 118, 59, 34, 225, 34, 104, 202, 192, 7, 66, 150, 137, 75, 106, 7, 22, 234, 42, 94, 139, 65, 241, 65, 1, 190, 153, 16, 221, 87]
                                    },
                            "lhs_pk": [206, 50, 185, 93, 20, 234, 100, 168, 163, 125, 95, 201, 162, 104, 35, 2, 205, 41, 180, 73, 107, 140, 79, 182, 173, 17, 172, 49, 51, 85, 180, 5],
                            "rhs_pk": [68, 40, 90, 110, 254, 68, 87, 12, 19, 21, 252, 197, 69, 255, 33, 172, 140, 70, 79, 39, 71, 217, 12, 254, 82, 125, 123, 148, 221, 217, 141, 194]
                        },
                        "lhs_pk": [155, 2, 30, 71, 52, 89, 112, 247, 108, 177, 144, 212, 206, 254, 87, 126, 180, 207, 146, 223, 164, 246, 178, 62, 148, 96, 39, 136, 106, 36, 253, 56],
                        "rhs_pk": [155, 140, 124, 154, 235, 97, 51, 77, 208, 24, 45, 219, 199, 232, 222, 26, 160, 62, 38, 253, 121, 241, 219, 233, 36, 50, 60, 182, 127, 255, 132, 245]
                    },
                    "lhs_pk": [172, 176, 18, 228, 203, 85, 44, 151, 221, 13, 91, 250, 67, 232, 114, 16, 251, 13, 115, 233, 214, 194, 102, 199, 200, 124, 30, 190, 143, 18, 85, 75],
                    "rhs_pk": [100, 192, 98, 123, 150, 116, 55, 42, 207, 44, 181, 31, 203, 65, 237, 13, 55, 246, 185, 211, 149, 245, 245, 219, 183, 41, 237, 253, 128, 231, 161, 226]
                },
                "lhs_pk": [112, 16, 177, 142, 158, 1, 36, 210, 87, 165, 5, 195, 199, 61, 13, 195, 219, 26, 231, 103, 163, 223, 54, 16, 106, 0, 252, 69, 242, 31, 210, 167],
                "rhs_pk": [15, 246, 81, 72, 172, 15, 170, 235, 10, 64, 229, 233, 169, 140, 179, 209, 244, 183, 3, 59, 2, 252, 233, 229, 13, 190, 196, 208, 109, 30, 73, 113]
            },
            "lhs_pk": [114, 238, 75, 184, 228, 147, 37, 72, 134, 65, 139, 64, 81, 114, 157, 148, 197, 108, 80, 89, 30, 235, 75, 108, 193, 53, 185, 15, 57, 61, 181, 119],
            "rhs_pk": [82, 28, 113, 114, 168, 192, 222, 110, 96, 15, 28, 179, 164, 180, 76, 87, 254, 72, 48, 154, 167, 102, 220, 74, 76, 136, 45, 105, 243, 87, 165, 212]
        }
    }
    "#;

    #[test]
    fn golden_initializer_deserialization() {
        let _: StmInitializerWrapper = serde_json::from_str(GOLDEN_STM_INITIALIZER_WRAPPER_JSON)
            .expect("Deserializing a StmInitializerWrapper should not fail");
    }

    #[test]
    fn test_initializer_wrapper_conversions() {
        let stm_initializer_wrapper_json = GOLDEN_STM_INITIALIZER_WRAPPER_JSON;

        let stm_initializer_wrapper_from_json: StmInitializerWrapper =
            serde_json::from_str(stm_initializer_wrapper_json)
                .expect("Deserializing a StmInitializerWrapper should not fail");
        let stm_initializer_wrapper_from_json_to_json =
            serde_json::to_string(&stm_initializer_wrapper_from_json)
                .expect("Serializing a StmInitializerWrapper to json should not fail");

        let stm_initializer_wrapper_from_bytes =
            StmInitializerWrapper::from_bytes(&stm_initializer_wrapper_from_json.to_bytes())
                .expect("Deserializing a StmInitializerWrapper from bytes should not fail");
        let stm_initializer_wrapper_from_bytes_to_json =
            serde_json::to_string(&stm_initializer_wrapper_from_bytes)
                .expect("Serializing a StmInitializerWrapper to json should not fail");

        assert_eq!(
            stm_initializer_wrapper_from_json_to_json,
            stm_initializer_wrapper_from_bytes_to_json
        );

        let mut stm_initializer_wrapper_from_json = stm_initializer_wrapper_from_json;
        stm_initializer_wrapper_from_json.kes_signature = None;

        let stm_initializer_wrapper_from_bytes =
            StmInitializerWrapper::from_bytes(&stm_initializer_wrapper_from_json.to_bytes())
                .expect("Deserializing a StmInitializerWrapper from bytes should not fail");
        assert_eq!(None, stm_initializer_wrapper_from_bytes.kes_signature);
    }
}
