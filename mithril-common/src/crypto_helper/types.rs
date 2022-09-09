use crate::crypto_helper::opcerts::OpCert;
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
    pub fn setup_new<R: RngCore + CryptoRng>(
        params: StmParameters,
        kes_sk: &[u8],
        kes_period: usize,
        stake: Stake,
        rng: &mut R,
    ) -> Self {
        let stm_initializer = StmInitializer::setup(params, stake, rng);
        let kes_sk: Sum6Kes = serde_cbor::from_slice(kes_sk).expect("Invalid KES key provided"); // todo: handle this
        let kes_signature = kes_sk.sign(kes_period, &stm_initializer.verification_key().to_bytes());

        Self {
            stm_initializer,
            kes_signature: Some(kes_signature),
        }
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
    pub fn register(
        &mut self,
        opcert: &[u8],
        kes_sig: Sum6KesSig,
        kes_period: usize,
        pk: ProtocolSignerVerificationKey,
    ) -> Result<(), RegisterError> {
        let cert = OpCert::parse(opcert)?;

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

        let sk_bytes_1 = Vec::from_hex("590260fe77acdfa56281e4b05198f5136018057a65f425411f0990cac4aca0f2917aa00a3d51e191f6f425d870aca3c6a2a41833621f5729d7bc0e3dfc3ae77d057e5e1253b71def7a54157b9f98973ca3c49edd9f311e5f4b23ac268b56a6ac040c14c6d2217925492e42f00dc89a2a01ff363571df0ca0db5ba37001cee56790cc01cd69c6aa760fca55a65a110305ea3c11da0a27be345a589329a584ebfc499c43c55e8c6db5d9c0b014692533ee78abd7ac1e79f7ec9335c7551d31668369b4d5111db78072f010043e35e5ca7f11acc3c05b26b9c7fe56f02aa41544f00cb7685e87f34c73b617260ade3c7b8d8c4df46693694998f85ad80d2cbab0b575b6ccd65d90574e84368169578bff57f751bc94f7eec5c0d7055ec88891a69545eedbfbd3c5f1b1c1fe09c14099f6b052aa215efdc5cb6cdc84aa810db41dbe8cb7d28f7c4beb75cc53915d3ac75fc9d0bf1c734a46e401e15150c147d013a938b7e07cc4f25a582b914e94783d15896530409b8acbe31ef471de8a1988ac78dfb7510729eff008084885f07df870b65e4f382ca15908e1dcda77384b5c724350de90cec22b1dcbb1cdaed88da08bb4772a82266ec154f5887f89860d0920dba705c45957ef6d93e42f6c9509c966277d368dd0eefa67c8147aa15d40a222f7953a4f34616500b310d00aa1b5b73eb237dc4f76c0c16813d321b2fc5ac97039be25b22509d1201d61f4ccc11cd4ff40fffe39f0e937b4722074d8e073a775d7283b715d46f79ce128e3f1362f35615fa72364d20b6db841193d96e58d9d8e86b516bbd1f05e45b39823a93f6e9f29d9e01acf2c12c072d1c64e0afbbabf6903ef542e").unwrap();
        let initializer_1 = ProtocolInitializer::setup_new(params, &sk_bytes_1, 0, 10, &mut rng);

        let cbor_bytes_1 = Vec::from_hex("82845820f89d3fa14cabafa151638743b297379d3c3767902e36ae53b02b3a64bddda19d00005840e472042f7e78e3cfc4c2ac99a658a626be0e9d69e7072dc300cb28ce8178c329beb1d2cf4c7a7ce30d6c528ffad9e8d685fd9d58379758924a010ef317290b0e58207acec462970b819f5f7951e5c84eb87c8e7c4f1aceac01e1c1d97f2e25eb6005").expect("Invalid Hex String");
        let key_registration_1 = key_reg
            .register(
                &cbor_bytes_1,
                initializer_1.kes_signature.unwrap(),
                0,
                initializer_1.stm_initializer.verification_key(),
            )
            .unwrap();
        // assert!(key_registration_1.is_ok());

        let sk_bytes_2 = Vec::from_hex("590260d12460c331c7b978887899e2ccb119df7c4fa1e6f396778518f64a58f3d2232bf6481fba48fef9dc796f604b622cf0b41230c1662c221b448142bfddf8170865f1deec4232322acffb7d9001f21a4985c4acb4ca6af8906aefd9a7de6bc360acb59812a12ebd5b36a5603b497061f983921cdce59c836ccd2172f40dd62902809316c9a10cf8c44d5a22606fbdea4e210ec1540c6f5c9c1269b2af47d76aae7b009936bf4b62d0716bedf39e589b82381013c5dd68bae7496c4c726b4ae3992c188f1055ab61ef4ce9858958040147d02cec38b058919f0fcebbb9e48c25561634b58dc2d168994e2e22926f4d9d073d0c333db92d19a0c08617e03091b66f7574275db8f2fed75c21a539afedb85b1f163589eb8950ceeb6fb06f022fe544329df46fee5eb495e514846e341f0dafb8cf0d216c0ef3f18ceba6cad155694b74ae39d05a0a4729e595c1bfefa110199ec90a62e79a69c7dca595088e1a9fa1dd4f12be8e6f9e69178478d857d18f1abfc6bf567f5fcfd19bdfcb41c9a65879940a336d41a22ca66c84ccee0afd88aa237e48cb4f6115847eff96c095a7a6672e7b712d9acb8eb59dd9a1ce0ca1bc720ee1613b6e159d8b903694621f095319ba2292760ea0b975f17b3a48fb251b722ef190aa23385d05de93b3a21ef563f64e6e66e6e199267141e417e5b8a74a80f8ae6e69a4d9e778e9b7ca79f04d992b3851dbf703cd69829c837339d831b60273610d002454060bc9e1637358cd1a593166fe328dad39b8fa412b81841429ae022e26c452dce5bcd6500d48bd22af4ff94264978c1874946ce4ad53afcf9bde71973ef744823179dd98e24cd40d9498c1").unwrap();
        let initializer_2 = ProtocolInitializer::setup_new(params, &sk_bytes_2, 0, 10, &mut rng);

        let cbor_bytes_2 = Vec::from_hex("82845820485981d05d157875fb5a69bbea8f6fc0c09c3da1754fb42a313d6f7a485ae2070000584024d23fed6746db342232e0ddab087879ade2bd936842af00b9c02f563d752cde583df1763283973ce527d118e79ae184c83a3a60a35898f23e9a441a2948240c5820ec91c9db4c0ffc5f39548b1eda07930b7a47510a0e1ce3cc621c6a3f9e899eda").expect("Invalid Hex String");
        let key_registration_2 = key_reg
            .register(
                &cbor_bytes_2,
                initializer_2.kes_signature.unwrap(),
                0,
                initializer_2.stm_initializer.verification_key(),
            )
            .unwrap();
        // assert!(key_registration.is_ok())
    }
}
