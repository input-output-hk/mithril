//! Key registration functionality.
use super::stm::Stake;
use crate::error::RegisterError;
use crate::merkle_tree::{MTLeaf, MerkleTree};
use crate::multi_sig::{VerificationKey, VerificationKeyPoP};
<<<<<<< HEAD
use blake2::digest::Digest;
=======
use blake2::VarBlake2b;
use digest::{Digest, FixedOutput, Update, VariableOutput};
use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use kes_summed_ed25519::kes::Sum6KesSig;
use kes_summed_ed25519::traits::KesSig;
>>>>>>> be47ea5e6 (PoolID working with the node)
use serde::{Deserialize, Serialize};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::Arc;

/// Stores a registered party with its public key and the associated stake.
pub type RegParty = MTLeaf;

/// Representation of the PoolID
pub type PoolId = [u8; 28];

/// Raw Fields of the operational certificates (without incluiding the cold VK)
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
struct RawFields(
    #[serde(with = "serde_bytes")] Vec<u8>,
    u64,
    u64,
    #[serde(with = "serde_bytes")] Vec<u8>,
);

/// Raw Operational Certificate
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
struct RawOpCert(RawFields, EdPublicKey);

/// Parsed Operational Certificate
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpCert {
    kes_vk: KesPublicKey,
    issue_number: u64,
    start_kes_period: u64, // this is not the kes period used in signing/verifying
    cert_sig: EdSignature,
    cold_vk: EdPublicKey,
}

/// NewKeyReg that is set to eventually replace `KeyReg`
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NewKeyReg {
    stake_distribution: HashMap<PoolId, Stake>,
    keys: HashMap<VerificationKey, Stake>,
}

/// Struct that collects public keys and stakes of parties.
/// Each participant (both the signers and the clerks) need to run their own instance of the key registration.
// todo: replace with KeyReg
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KeyReg {
    keys: HashMap<VerificationKey, Stake>,
}

/// Structure generated out of a closed registration containing the registered parties, total stake, and the merkle tree.
/// One can only get a global `avk` out of a closed key registration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosedKeyReg<D: Digest> {
    /// Ordered list of registered parties.
    pub reg_parties: Vec<RegParty>,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub merkle_tree: Arc<MerkleTree<D>>,
}

impl OpCert {
    /// Parse raw bytes into an Operational Certificate
    pub fn parse(bytes: &[u8]) -> Result<Self, RegisterError> {
        let a: RawOpCert =
            serde_cbor::from_slice(bytes).map_err(|_| RegisterError::SerializationError)?;

        Ok(Self {
            kes_vk: KesPublicKey::from_bytes(&a.0 .0)
                .map_err(|_| RegisterError::SerializationError)?,
            issue_number: a.0 .1,
            start_kes_period: a.0 .2,
            cert_sig: EdSignature::from_bytes(&a.0 .3)
                .map_err(|_| RegisterError::SerializationError)?,
            cold_vk: a.1,
        })
    }

    /// Validate a certificate
    pub fn validate(&self) -> Result<(), RegisterError> {
        let mut msg = [0u8; 48];
        msg[..32].copy_from_slice(self.kes_vk.as_bytes());
        msg[32..40].copy_from_slice(&self.issue_number.to_be_bytes());
        msg[40..48].copy_from_slice(&self.start_kes_period.to_be_bytes());

        if self.cold_vk.verify(&msg, &self.cert_sig).is_ok() {
            return Ok(());
        }

        Err(RegisterError::InvalidOpCert)
    }
}

impl NewKeyReg {
    /// New Initialisation function. We temporarily keep the other init function,
    /// but we should eventually transition to only use this one.
    pub fn init(stake_dist: &[(PoolId, Stake)]) -> Self {
        Self {
            stake_distribution: HashMap::from_iter(stake_dist.to_vec()),
            keys: HashMap::new(),
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
        pk: VerificationKeyPoP,
    ) -> Result<(), RegisterError> {
        let cert = OpCert::parse(opcert)?;

        cert.validate().map_err(|_| RegisterError::InvalidOpCert)?;
        kes_sig
            .verify(kes_period, &cert.kes_vk, &pk.to_bytes())
            .map_err(|_| RegisterError::KesSignatureInvalid)?;

        let mut hasher = VarBlake2b::new(28).unwrap();
        hasher.update(cert.cold_vk.as_bytes());
        let mut pool_id = [0u8; 28];
        hasher.finalize_variable(|res| {
            pool_id.copy_from_slice(res);
        });

        if let Some(&stake) = self.stake_distribution.get(&pool_id) {
            if let Entry::Vacant(e) = self.keys.entry(pk.vk) {
                if pk.check().is_ok() {
                    e.insert(stake);
                    return Ok(());
                } else {
                    return Err(RegisterError::KeyInvalid(Box::new(pk)));
                }
            }
            return Err(RegisterError::KeyRegistered(Box::new(pk.vk)));
        }
        Err(RegisterError::KeyNonExisting)
    }
}

impl KeyReg {
    /// Initialise an empty `KeyReg`.
    /// todo: remove this init function
    pub fn init() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    /// Verify and register a public key and stake for a particular party.
    /// # Error
    /// The function fails when the proof of possession is invalid or when the key is already registered.
    pub fn register(&mut self, stake: Stake, pk: VerificationKeyPoP) -> Result<(), RegisterError> {
        if let Entry::Vacant(e) = self.keys.entry(pk.vk) {
            if pk.check().is_ok() {
                e.insert(stake);
                return Ok(());
            } else {
                return Err(RegisterError::KeyInvalid(Box::new(pk)));
            }
        }
        Err(RegisterError::KeyRegistered(Box::new(pk.vk)))
    }

    /// Finalize the key registration.
    /// This function disables `KeyReg::register`, consumes the instance of `self`, and returns a `ClosedKeyReg`.
    pub fn close<D: Digest>(self) -> ClosedKeyReg<D> {
        let mut total_stake: Stake = 0;
        let mut reg_parties = self
            .keys
            .iter()
            .map(|(&vk, &stake)| {
                let (res, overflow) = total_stake.overflowing_add(stake);
                if overflow {
                    panic!("Total stake overflow");
                }
                total_stake = res;
                MTLeaf(vk, stake)
            })
            .collect::<Vec<RegParty>>();
        reg_parties.sort();

        ClosedKeyReg {
            merkle_tree: Arc::new(MerkleTree::create(&reg_parties)),
            reg_parties,
            total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::multi_sig::SigningKey;
    use crate::stm::{StmInitializer, StmParameters};
<<<<<<< HEAD
    use blake2::{digest::consts::U32, Blake2b};
=======
    use blake2::Blake2b;
>>>>>>> be47ea5e6 (PoolID working with the node)
    use hex::FromHex;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    proptest! {
        #[test]
        fn test_keyreg(stake in vec(1..1u64 << 60, 2..=10),
                       nkeys in 2..10_usize,
                       fake_it in 0..4usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyReg::init();

            let gen_keys = (1..nkeys).map(|_| {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
            }).collect::<Vec<_>>();

            let fake_key = {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
            };

            // Record successful registrations
            let mut keys = HashMap::new();

            for (i, &stake) in stake.iter().enumerate() {
                let mut pk = gen_keys[i % gen_keys.len()];

                if fake_it == 0 {
                    pk.pop = fake_key.pop;
                }

                let reg = kr.register(stake, pk);
                match reg {
                    Ok(_) => {
                        assert!(keys.insert(pk.vk, stake).is_none());
                    },
                    Err(RegisterError::KeyRegistered(pk1)) => {
                        assert!(pk1.as_ref() == &pk.vk);
                        assert!(keys.contains_key(&pk.vk));
                    }
                    Err(RegisterError::KeyInvalid(a)) => {
                        assert_eq!(fake_it, 0);
                        assert!(a.check().is_err());
                    }
                    Err(RegisterError::SerializationError) => unreachable!(),
                    _ => unreachable!(),
                }
            }

            if !kr.keys.is_empty() {
                let closed = kr.close::<Blake2b<U32>>();
                let retrieved_keys = closed.reg_parties.iter().map(|r| (r.0, r.1)).collect::<HashMap<_,_>>();
                assert!(retrieved_keys == keys);
            }
        }
    }

    #[test]
    fn test_vector_op_cert() {
        let cbor_bytes = Vec::from_hex("8284582067fd5ccf770c0182a34d2b3d2011ca3a853ba947e17cae7543e668bc7687eb6a0000584050592bef1c630f2df499161d78bfadb44cc76cfd24048993ace4a45dade37b4f29e95172fde4e63581a93552f6986985616b70f61062a1db2ee0d3d8e671440e58202abf3ff537a2080f53fa38615906fa6094d44860902f2b2dffdbb41b811ff39f").expect("Invalid Hex String");
        let cert = OpCert::parse(&cbor_bytes).unwrap();

        assert!(cert.validate().is_ok())
    }

    #[test]
    fn test_vector_key_reg() {
        let params = StmParameters {
            m: 5,
            k: 5,
            phi_f: 1.0,
        };
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

<<<<<<< HEAD
        let fake_pool_id = [
            134, 128, 179, 254, 245, 165, 179, 39, 71, 156, 226, 254, 129, 15, 231, 1, 142, 176,
            236, 148, 207, 175, 146, 72, 222, 186, 20, 75,
        ];
        let mut key_reg = NewKeyReg::init(&[(fake_pool_id, 10)]);
=======
        let mut pool_id_1 = [0; 28];
        pool_id_1.copy_from_slice(
            &Vec::from_hex("290de5c13d3d1e05255895915eff06331f546e9239be6b37767f5ae2").unwrap(),
        );
>>>>>>> be47ea5e6 (PoolID working with the node)

        let mut pool_id_2 = [0; 28];
        pool_id_2.copy_from_slice(
            &Vec::from_hex("d8b8d8fea0d08c4e2d4c1120ba746ad951ee62a5929d5fe90ee20e4d").unwrap(),
        );
        let mut key_reg = NewKeyReg::init(&[(pool_id_1, 10), (pool_id_2, 3)]);

        let sk_bytes_1 = Vec::from_hex("590260fe77acdfa56281e4b05198f5136018057a65f425411f0990cac4aca0f2917aa00a3d51e191f6f425d870aca3c6a2a41833621f5729d7bc0e3dfc3ae77d057e5e1253b71def7a54157b9f98973ca3c49edd9f311e5f4b23ac268b56a6ac040c14c6d2217925492e42f00dc89a2a01ff363571df0ca0db5ba37001cee56790cc01cd69c6aa760fca55a65a110305ea3c11da0a27be345a589329a584ebfc499c43c55e8c6db5d9c0b014692533ee78abd7ac1e79f7ec9335c7551d31668369b4d5111db78072f010043e35e5ca7f11acc3c05b26b9c7fe56f02aa41544f00cb7685e87f34c73b617260ade3c7b8d8c4df46693694998f85ad80d2cbab0b575b6ccd65d90574e84368169578bff57f751bc94f7eec5c0d7055ec88891a69545eedbfbd3c5f1b1c1fe09c14099f6b052aa215efdc5cb6cdc84aa810db41dbe8cb7d28f7c4beb75cc53915d3ac75fc9d0bf1c734a46e401e15150c147d013a938b7e07cc4f25a582b914e94783d15896530409b8acbe31ef471de8a1988ac78dfb7510729eff008084885f07df870b65e4f382ca15908e1dcda77384b5c724350de90cec22b1dcbb1cdaed88da08bb4772a82266ec154f5887f89860d0920dba705c45957ef6d93e42f6c9509c966277d368dd0eefa67c8147aa15d40a222f7953a4f34616500b310d00aa1b5b73eb237dc4f76c0c16813d321b2fc5ac97039be25b22509d1201d61f4ccc11cd4ff40fffe39f0e937b4722074d8e073a775d7283b715d46f79ce128e3f1362f35615fa72364d20b6db841193d96e58d9d8e86b516bbd1f05e45b39823a93f6e9f29d9e01acf2c12c072d1c64e0afbbabf6903ef542e").unwrap();
        let initializer_1 = StmInitializer::setup_new(params, &sk_bytes_1, 0, 10, &mut rng);

        let cbor_bytes_1 = Vec::from_hex("82845820f89d3fa14cabafa151638743b297379d3c3767902e36ae53b02b3a64bddda19d00005840e472042f7e78e3cfc4c2ac99a658a626be0e9d69e7072dc300cb28ce8178c329beb1d2cf4c7a7ce30d6c528ffad9e8d685fd9d58379758924a010ef317290b0e58207acec462970b819f5f7951e5c84eb87c8e7c4f1aceac01e1c1d97f2e25eb6005").expect("Invalid Hex String");
        assert!(key_reg
            .register(
                &cbor_bytes_1,
                initializer_1.kes_sig.unwrap(),
                0,
                initializer_1.pk
            )
            .is_ok());

        let sk_bytes_2 = Vec::from_hex("590260d12460c331c7b978887899e2ccb119df7c4fa1e6f396778518f64a58f3d2232bf6481fba48fef9dc796f604b622cf0b41230c1662c221b448142bfddf8170865f1deec4232322acffb7d9001f21a4985c4acb4ca6af8906aefd9a7de6bc360acb59812a12ebd5b36a5603b497061f983921cdce59c836ccd2172f40dd62902809316c9a10cf8c44d5a22606fbdea4e210ec1540c6f5c9c1269b2af47d76aae7b009936bf4b62d0716bedf39e589b82381013c5dd68bae7496c4c726b4ae3992c188f1055ab61ef4ce9858958040147d02cec38b058919f0fcebbb9e48c25561634b58dc2d168994e2e22926f4d9d073d0c333db92d19a0c08617e03091b66f7574275db8f2fed75c21a539afedb85b1f163589eb8950ceeb6fb06f022fe544329df46fee5eb495e514846e341f0dafb8cf0d216c0ef3f18ceba6cad155694b74ae39d05a0a4729e595c1bfefa110199ec90a62e79a69c7dca595088e1a9fa1dd4f12be8e6f9e69178478d857d18f1abfc6bf567f5fcfd19bdfcb41c9a65879940a336d41a22ca66c84ccee0afd88aa237e48cb4f6115847eff96c095a7a6672e7b712d9acb8eb59dd9a1ce0ca1bc720ee1613b6e159d8b903694621f095319ba2292760ea0b975f17b3a48fb251b722ef190aa23385d05de93b3a21ef563f64e6e66e6e199267141e417e5b8a74a80f8ae6e69a4d9e778e9b7ca79f04d992b3851dbf703cd69829c837339d831b60273610d002454060bc9e1637358cd1a593166fe328dad39b8fa412b81841429ae022e26c452dce5bcd6500d48bd22af4ff94264978c1874946ce4ad53afcf9bde71973ef744823179dd98e24cd40d9498c1").unwrap();
        let initializer_2 = StmInitializer::setup_new(params, &sk_bytes_2, 0, 10, &mut rng);

        let cbor_bytes_2 = Vec::from_hex("82845820485981d05d157875fb5a69bbea8f6fc0c09c3da1754fb42a313d6f7a485ae2070000584024d23fed6746db342232e0ddab087879ade2bd936842af00b9c02f563d752cde583df1763283973ce527d118e79ae184c83a3a60a35898f23e9a441a2948240c5820ec91c9db4c0ffc5f39548b1eda07930b7a47510a0e1ce3cc621c6a3f9e899eda").expect("Invalid Hex String");
        assert!(key_reg
            .register(
                &cbor_bytes_2,
                initializer_2.kes_sig.unwrap(),
                0,
                initializer_2.pk
            )
            .is_ok());
    }
}
