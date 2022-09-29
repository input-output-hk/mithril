use crate::crypto_helper::cardano::{FromShelleyFile, OpCert, ParseError};
use crate::crypto_helper::types::{
    ProtocolAggregateVerificationKey, ProtocolPartyId, ProtocolSignerVerificationKey,
    ProtocolSignerVerificationKeySignature, ProtocolStakeDistribution,
};

use mithril::key_reg::{ClosedKeyReg, KeyReg};
use mithril::stm::{
    Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer, StmParameters, StmSig,
    StmSigner, StmVerificationKeyPoP,
};
use mithril::{AggregationError, RegisterError};

use blake2::{digest::consts::U32, Blake2b, Digest};
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
pub type KESPeriod = usize;

/// New registration error
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ProtocolRegistrationErrorWrapper {
    /// Error raised when an operational certificate is invalid
    #[error("invalid operational certificate")]
    OpCertInvalid,

    /// Error raised when a KES Signature verification fails
    #[error("KES signature verification error")]
    KesSignatureInvalid,

    /// Error raised when a pool address encoding fails
    #[error("pool address encoding error")]
    PoolAddressEncoding,

    /// Error raised when a core registration error occurs
    #[error("genesis signature verification error: '{0}'")]
    CoreRegister(#[from] RegisterError),
}

// Wrapper structures to reduce library misuse in the Cardano context
/// Wrapper structure for [MithrilCore:StmInitializer](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmInitializer.html).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializerWrapper {
    stm_initializer: StmInitializer,
    kes_signature: Option<ProtocolSignerVerificationKeySignature>, // todo: The option is ONLY for a smooth transition. We have to remove this.
}

/// Wrapper structure for [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html).
#[derive(Debug, Clone)]
pub struct KeyRegWrapper {
    stm_key_reg: KeyReg,
    stake_distribution: HashMap<ProtocolPartyId, Stake>,
}

/// Wrapper structure for [MithrilCore:StmSigner](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSigner.html).
#[derive(Debug, Clone)]
pub struct StmSignerWrapper(StmSigner<D>);

/// Wrapper structure fo [MithrilCore:StmClerk](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmClerk.html).
#[derive(Debug, Clone)]
pub struct StmClerkWrapper(StmClerk<D>);

impl StmClerkWrapper {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn from_registration(params: &StmParameters, closed_reg: &ClosedKeyReg<D>) -> Self {
        Self(StmClerk::from_registration(params, closed_reg))
    }

    /// Create a Clerk from a signer.
    pub fn from_signer(signer: &StmSignerWrapper) -> Self {
        Self(StmClerk::from_signer(&signer.0))
    }

    /// Aggregate a set of signatures for their corresponding indices.
    pub fn aggregate(
        &self,
        sigs: &[StmSig<D>],
        msg: &[u8],
    ) -> Result<StmAggrSig<D>, AggregationError> {
        self.0.aggregate(sigs, msg)
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration.
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        self.0.compute_avk()
    }
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
    ) -> Result<Self, ParseError> {
        let stm_initializer = StmInitializer::setup(params, stake, rng);
        let kes_signature = if let Some(kes_sk_path) = kes_sk_path {
            let kes_sk: Sum6Kes = Sum6Kes::from_file(kes_sk_path)?;
            Some(kes_sk.sign(
                kes_period.unwrap_or_default(),
                &stm_initializer.verification_key().to_bytes(),
            ))
        } else {
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
        self.kes_signature.clone()
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
    ) -> Result<StmSignerWrapper, ProtocolRegistrationErrorWrapper> {
        Ok(StmSignerWrapper(
            self.stm_initializer.new_signer(closed_reg)?,
        ))
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
        kes_period: KESPeriod,
        pk: ProtocolSignerVerificationKey,
    ) -> Result<ProtocolPartyId, ProtocolRegistrationErrorWrapper> {
        let pool_id_bech32: ProtocolPartyId = if let Some(opcert) = opcert {
            opcert
                .validate()
                .map_err(|_| ProtocolRegistrationErrorWrapper::OpCertInvalid)?;
            kes_sig
                .unwrap()
                .verify(kes_period, &opcert.kes_vk, &pk.to_bytes())
                .map_err(|_| ProtocolRegistrationErrorWrapper::KesSignatureInvalid)?;
            opcert
                .compute_protocol_party_id()
                .map_err(|_| ProtocolRegistrationErrorWrapper::PoolAddressEncoding)?
        } else {
            println!("WARNING: Signer certification is skipped!");
            party_id.unwrap()
        };

        if let Some(&stake) = self.stake_distribution.get(&pool_id_bech32) {
            self.stm_key_reg
                .register(stake, pk)
                .map_err(ProtocolRegistrationErrorWrapper::CoreRegister)?;
            return Ok(pool_id_bech32);
        }

        Err(ProtocolRegistrationErrorWrapper::CoreRegister(
            RegisterError::KeyNonExisting,
        ))
    }

    /// Finalize the key registration.
    /// This function disables `KeyReg::register`, consumes the instance of `self`, and returns a `ClosedKeyReg`.
    pub fn close<D: Digest>(self) -> ClosedKeyReg<D> {
        self.stm_key_reg.close()
    }
}

impl StmSignerWrapper {
    /// This function produces an STM signature
    pub fn sign(&self, msg: &[u8]) -> Option<StmSig<D>> {
        self.0.sign(msg)
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration, which consists of
    /// the merkle tree root and the total stake.
    pub fn compute_avk(&self) -> ProtocolAggregateVerificationKey {
        self.0.compute_avk()
    }
}

#[cfg(all(test))]
mod test {
    use super::*;
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
        let pool_id_1 = "pool19yx7tsfa850q2f2cjkg4alcxxv04gm5j8xlxkdmk0adwylsdrta".to_string();
        let pool_id_2 = "pool1mzud3l4q6zxyut2vzyst5ar2m9g7uc49j2w4l6gwug8y6h3s7k4".to_string();
        let mut key_reg = KeyRegWrapper::init(&vec![(pool_id_1, 10), (pool_id_2, 3)]);

        let initializer_1 = StmInitializerWrapper::setup(
            params,
            Some("./test-data/kes1.skey"),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert1: OpCert = OpCert::from_file("./test-data/node1.cert")
            .expect("opcert deserialization should not fail");

        let key_registration_1 = key_reg.register(
            None,
            Some(opcert1),
            initializer_1.kes_signature,
            0,
            initializer_1.stm_initializer.verification_key(),
        );
        assert!(key_registration_1.is_ok());

        let initializer_2 = StmInitializerWrapper::setup(
            params,
            Some("./test-data/kes2.skey"),
            Some(0),
            10,
            &mut rng,
        )
        .unwrap();

        let opcert2: OpCert = OpCert::from_file("./test-data/node2.cert")
            .expect("opcert deserialization should not fail");

        let key_registration_2 = key_reg.register(
            None,
            Some(opcert2),
            initializer_2.kes_signature,
            0,
            initializer_2.stm_initializer.verification_key(),
        );
        assert!(key_registration_2.is_ok())
    }
}
