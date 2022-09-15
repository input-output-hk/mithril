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

use bech32::{self, ToBase32, Variant};
use blake2::{
    digest::consts::{U28, U32},
    Blake2b, Digest,
};
use kes_summed_ed25519::kes::{Sum6Kes, Sum6KesSig};
use kes_summed_ed25519::traits::{KesSig, KesSk};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

// Protocol types alias
type D = Blake2b<U32>;

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
    /// Old setup. todo: remove
    pub fn setup<R: RngCore + CryptoRng>(params: StmParameters, stake: Stake, rng: &mut R) -> Self {
        Self {
            stm_initializer: StmInitializer::setup(params, stake, rng),
            kes_signature: None,
        }
    }
    /// Builds an `StmInitializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, signs the verification
    /// key with a provided KES signing key, and initialises the structure.
    pub fn setup_new<R: RngCore + CryptoRng, P: AsRef<Path>>(
        params: StmParameters,
        kes_sk_path: P,
        kes_period: usize,
        stake: Stake,
        rng: &mut R,
    ) -> Result<Self, ParseError> {
        let stm_initializer = StmInitializer::setup(params, stake, rng);
        let kes_sk: Sum6Kes = Sum6Kes::from_file(kes_sk_path)?;
        let kes_signature = kes_sk.sign(kes_period, &stm_initializer.verification_key().to_bytes());

        Ok(Self {
            stm_initializer,
            kes_signature: Some(kes_signature),
        })
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKeyPoP {
        self.stm_initializer.verification_key()
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
    ) -> Result<StmSignerWrapper, RegisterError> {
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
    pub fn register<P: AsRef<Path>>(
        &mut self,
        opcert_path: P,
        kes_sig: ProtocolSignerVerificationKeySignature,
        kes_period: usize,
        pk: ProtocolSignerVerificationKey,
    ) -> Result<(), RegisterError> {
        let cert = OpCert::from_file(opcert_path)?;

        cert.validate().map_err(|_| RegisterError::InvalidOpCert)?;
        kes_sig
            .verify(kes_period, &cert.kes_vk, &pk.to_bytes())
            .map_err(|_| RegisterError::KesSignatureInvalid)?;

        let mut hasher = Blake2b::<U28>::new();
        hasher.update(cert.cold_vk.as_bytes());
        let mut pool_id = [0u8; 28];
        pool_id.copy_from_slice(hasher.finalize().as_slice());
        let pool_id_bech32 = bech32::encode("pool", pool_id.to_base32(), Variant::Bech32)
            .map_err(|_| RegisterError::InvalidOpCert)?; // TODO: maybe this should be a different error type like `RegisterError::InvalidPoolAddress`

        if let Some(&stake) = self.stake_distribution.get(&pool_id_bech32) {
            return self.stm_key_reg.register(stake, pk);
        }

        Err(RegisterError::KeyNonExisting)
    }
}

impl StmSignerWrapper {
    /// This function produces an STM signature
    pub fn sign(&self, msg: &[u8]) -> Option<StmSig<D>> {
        self.0.sign(msg)
    }

    /// This function should be called when a signing epoch is finished (or when a new one starts).
    /// It consumes `self` and turns it back to an `StmInitializer`, which allows for an update in
    /// the dynamic parameters (such as stake distribution, participants or KES signature). To ensure
    /// that the `StmInitializer` will not be used for the previous registration, this function also
    /// consumes the `ClosedKeyReg` instance. In case the stake of the current party has changed, it
    /// includes it as input.
    pub fn new_epoch(
        self,
        new_kes_key: &[u8],
        new_kes_period: usize,
        new_stake: Option<Stake>,
    ) -> StmInitializerWrapper {
        let stm_initializer = self.0.new_epoch(new_stake);

        let kes_sk: Sum6Kes =
            serde_cbor::from_slice(new_kes_key).expect("Invalid KES key provided"); // todo: handle this
        let kes_signature = kes_sk.sign(
            new_kes_period,
            &stm_initializer.verification_key().to_bytes(),
        );

        StmInitializerWrapper {
            stm_initializer,
            kes_signature: Some(kes_signature),
        }
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration, which consists of
    /// the merkle tree root and the total stake.
    pub fn compute_avk(&self) -> ProtocolAggregateVerificationKey {
        self.0.compute_avk()
    }
}

#[cfg(test)]
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

        let initializer_1 =
            StmInitializerWrapper::setup_new(params, "./test-data/kes1.skey", 0, 10, &mut rng)
                .unwrap();

        let key_registration_1 = key_reg.register(
            "./test-data/node1.cert",
            initializer_1.kes_signature.unwrap(),
            0,
            initializer_1.stm_initializer.verification_key(),
        );
        assert!(key_registration_1.is_ok());

        let initializer_2 =
            StmInitializerWrapper::setup_new(params, "./test-data/kes2.skey", 0, 10, &mut rng)
                .unwrap();

        let key_registration_2 = key_reg.register(
            "./test-data/node2.cert",
            initializer_2.kes_signature.unwrap(),
            0,
            initializer_2.stm_initializer.verification_key(),
        );
        assert!(key_registration_2.is_ok())
    }
}
