use crate::crypto_helper::shelley_types::{FromShelleyFile, OpCert, ParseError};
use blake2::{
    digest::consts::{U28, U32},
    Blake2b, Digest,
};
use ed25519_dalek;
use kes_summed_ed25519::kes::{Sum6Kes, Sum6KesSig};
use kes_summed_ed25519::traits::{KesSig, KesSk};
use mithril::key_reg::{ClosedKeyReg, KeyReg};
use mithril::stm::{
    Index, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer, StmParameters,
    StmSig, StmSigner, StmVerificationKeyPoP,
};
use mithril::{AggregationError, RegisterError};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// A protocol version
pub type ProtocolVersion<'a> = &'a str;

/// Representation of the PoolID
pub type PoolId = [u8; 28];

// Protocol types alias
type D = Blake2b<U32>;
/// The id of a mithril party.
pub type ProtocolPartyId = String;
/// Alias of [MithrilCore:Stake](https://mithril.network/mithril-core/doc/mithril/stm/type.Stake.html).
pub type ProtocolStake = Stake;
/// A list of [Party Id][ProtocolPartyId] associated with its [Stake][ProtocolStake].
pub type ProtocolStakeDistribution = Vec<(ProtocolPartyId, ProtocolStake)>; // todo: should eventually be Vec<(PoolId, ProtocolStake)>
/// Alias of [MithrilCore::StmParameters](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmParameters.html).
pub type ProtocolParameters = StmParameters;
/// Alias of [MithrilCore::Index](https://mithril.network/mithril-core/doc/mithril/stm/type.Index.html).
pub type ProtocolLotteryIndex = Index;
/// Alias of [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html).
pub type ProtocolKeyRegistration = KeyReg;
/// Alias of [MithrilCore:StmSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSig.html).
pub type ProtocolSingleSignature = StmSig<D>;
/// Alias of [MithrilCore:StmAggrSig](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrSig.html).
pub type ProtocolMultiSignature = StmAggrSig<D>;
/// Alias of [MithrilCore:StmVerificationKeyPoP](https://mithril.network/mithril-core/doc/mithril/stm/type.StmVerificationKeyPoP.html).
pub type ProtocolSignerVerificationKey = StmVerificationKeyPoP;
/// Alias of [MithrilCore:StmAggrVerificationKey](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmAggrVerificationKey.html).
pub type ProtocolAggregateVerificationKey = StmAggrVerificationKey<D>;
/// Alias of [Ed25519:PublicKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.PublicKey.html).
pub type ProtocolGenesisVerificationKey = ed25519_dalek::PublicKey;
/// Alias of [Ed25519:SecretKey](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.SecretKey.html).
pub type ProtocolGenesisSecretKey = ed25519_dalek::SecretKey;
/// Alias of [Ed25519:Signature](https://docs.rs/ed25519-dalek/latest/ed25519_dalek/struct.Signature.html).
pub type ProtocolGenesisSignature = ed25519_dalek::Signature;

// Error alias
/// Alias of [MithrilCore:RegisterError](https://mithril.network/mithril-core/doc/mithril/error/enum.RegisterError.html).
pub type ProtocolRegistrationError = RegisterError;
/// Alias of [MithrilCore:AggregationError](https://mithril.network/mithril-core/doc/mithril/error/enum.AggregationError.html).
pub type ProtocolAggregationError = AggregationError;

// Wrapper structures to reduce library misuse in the Cardano context
/// Wrapper structure for [MithrilCore:StmInitializer](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmInitializer.html).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolInitializer {
    stm_initializer: StmInitializer,
    kes_signature: Option<Sum6KesSig>, // todo: The option is ONLY for a smooth transition. We have to remove this.
}

/// Wrapper structure for [MithrilCore:KeyReg](https://mithril.network/mithril-core/doc/mithril/key_reg/struct.KeyReg.html).
#[derive(Debug, Clone)]
pub struct NewProtocolKeyRegistration {
    stm_key_reg: KeyReg,
    stake_distribution: HashMap<PoolId, Stake>,
}

/// Wrapper structure for [MithrilCore:StmSigner](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmSigner.html).
#[derive(Debug, Clone)]
pub struct ProtocolSigner(StmSigner<D>);

/// Wrapper structure fo [MithrilCore:StmClerk](https://mithril.network/mithril-core/doc/mithril/stm/struct.StmClerk.html).
#[derive(Debug, Clone)]
pub struct ProtocolClerk(StmClerk<D>);

impl ProtocolClerk {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn from_registration(params: &StmParameters, closed_reg: &ClosedKeyReg<D>) -> Self {
        Self(StmClerk::from_registration(params, closed_reg))
    }

    /// Create a Clerk from a signer.
    pub fn from_signer(signer: &ProtocolSigner) -> Self {
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

impl FromShelleyFile for Sum6Kes {}

impl ProtocolInitializer {
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
    /// Returns a `ProtocolSigner` specialized to
    /// * this `ProtocolSigner`'s ID and current stake
    /// * this `ProtocolSigner`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    pub fn new_signer(self, closed_reg: ClosedKeyReg<D>) -> Result<ProtocolSigner, RegisterError> {
        Ok(ProtocolSigner(self.stm_initializer.new_signer(closed_reg)?))
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

    /// Convert a slice of bytes to an `ProtocolInitializer`
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

impl NewProtocolKeyRegistration {
    /// New Initialisation function. We temporarily keep the other init function,
    /// but we should eventually transition to only use this one.
    pub fn init(stake_dist: &[(PoolId, Stake)]) -> Self {
        Self {
            stm_key_reg: ProtocolKeyRegistration::init(),
            stake_distribution: HashMap::from_iter(stake_dist.to_vec()),
        }
    }

    /// Register a new party. For a successful registration, the registrar needs to
    /// provide the OpCert (in cbor form), the cold VK, a KES signature, and a
    /// Mithril key (with its corresponding Proof of Possession).
    pub fn register<P: AsRef<Path>>(
        &mut self,
        opcert_path: P,
        kes_sig: Sum6KesSig,
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

        if let Some(&stake) = self.stake_distribution.get(&pool_id) {
            return self.stm_key_reg.register(stake, pk);
        }

        Err(RegisterError::KeyNonExisting)
    }
}

impl ProtocolSigner {
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
    ) -> ProtocolInitializer {
        let stm_initializer = self.0.new_epoch(new_stake);

        let kes_sk: Sum6Kes =
            serde_cbor::from_slice(new_kes_key).expect("Invalid KES key provided"); // todo: handle this
        let kes_signature = kes_sk.sign(
            new_kes_period,
            &stm_initializer.verification_key().to_bytes(),
        );

        ProtocolInitializer {
            stm_initializer,
            kes_signature: Some(kes_signature),
        }
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration, which consists of
    /// the merkle tree root and the total stake.
    pub fn compute_avk(&self) -> ProtocolAggregateVerificationKey {
        ProtocolAggregateVerificationKey::from(&self.0.get_closed_reg())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hex::FromHex;
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

        let mut pool_id_1 = [0; 28];
        pool_id_1.copy_from_slice(
            &Vec::from_hex("290de5c13d3d1e05255895915eff06331f546e9239be6b37767f5ae2").unwrap(),
        );

        let mut pool_id_2 = [0; 28];
        pool_id_2.copy_from_slice(
            &Vec::from_hex("d8b8d8fea0d08c4e2d4c1120ba746ad951ee62a5929d5fe90ee20e4d").unwrap(),
        );
        let mut key_reg = NewProtocolKeyRegistration::init(&[(pool_id_1, 10), (pool_id_2, 3)]);

        let initializer_1 =
            ProtocolInitializer::setup_new(params, "./test-data/kes1.skey", 0, 10, &mut rng)
                .unwrap();

        let key_registration_1 = key_reg.register(
            "./test-data/node1.cert",
            initializer_1.kes_signature.unwrap(),
            0,
            initializer_1.stm_initializer.verification_key(),
        );
        assert!(key_registration_1.is_ok());

        let initializer_2 =
            ProtocolInitializer::setup_new(params, "./test-data/kes2.skey", 0, 10, &mut rng)
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
