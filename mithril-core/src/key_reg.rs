//! Key registration functionality.
use crate::error::RegisterError;
use crate::multi_sig::{VerificationKey, VerificationKeyPoP};
use blake2::digest::Digest;
use serde::{Deserialize, Serialize};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::Arc;

use super::stm::Stake;
use crate::merkle_tree::{MTLeaf, MerkleTree};
use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use kes_summed_ed25519::kes::Sum6KesSig;
use kes_summed_ed25519::traits::KesSig;

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
    pub kes_vk: KesPublicKey,
    issue_number: u64,
    start_kes_period: u64, // this is not the kes period used in signing/verifying
    cert_sig: EdSignature,
    pub cold_vk: EdPublicKey,
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

        let mut pool_id = [0u8; 28];
        pool_id.copy_from_slice(&blake2::Blake2b::digest(cert.cold_vk.as_bytes()).as_slice()[..28]);

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
    use blake2::{digest::consts::U32, Blake2b};
    use hex::FromHex;
    use kes_summed_ed25519::kes::Sum6Kes;
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

        let fake_pool_id = [
            134, 128, 179, 254, 245, 165, 179, 39, 71, 156, 226, 254, 129, 15, 231, 1, 142, 176,
            236, 148, 207, 175, 146, 72, 222, 186, 20, 75,
        ];
        let mut key_reg = NewKeyReg::init(&[(fake_pool_id, 10)]);

        let cbor_sk_bytes = Vec::from_hex("78c158ae1edb8b4a2f1866f61e2a69a4595167be8cbdf57bab2361cd47ba7a0ee137ed17b49e5e9cd2674a451fadc87933cdd0473e4c38c1e0faca4f6c9cb482b854d25266877abb2a5bd8e111db383699aacac7049945d2f893403f65a4d8b34d1d9807dfe57b9a207c9d2c08d0d3478d292b2b8cdd9c7d1eb4225581bd12ad5d2c1ad357a0eba6bf2761787ff78fd97813d9287a57762c1710bd7b6a0b237fc875868e4218d0f703097bd89c776717a7461b2dcd8aac7095f0d68b065eccedd433f643903750b98349249a5c3ced7a0dc0e21af54c7da1be010ef6da6dc3209dea6e1f0ffaf1706b914d3bd94255b67b36f5150a3490d731863621f2edd3bb320a0ce737a83895b6ed80684542b70690db2e1efbaefe8e6cd0cd3a0fd84e0f37524ffd5f4647bc15dc9f3efa4f3ec53d1ff8fdd47179498e6d5661521c064810ab1e2af98737f7d79ec1b21faf98a5d0eef6b6152223dc0aaf3599979e6932c18ec18f58fd6bfc055d8fe3f4932f6edf64a40d3bcc466a77c598a8498d1d206c79eb6172cba9f4e724c80487df559bacc51941ee9c0025de3b98256ef4f54f776e575a926a90a1b68cea5856ea92d95dd7d2088a8ee8c6d042cb9bc454301317a29e15b49a0ceddbbe8097cdf230df64220ead235b6c6c125ffef36b4f8fda6742dd850af8271a7a9df84a240dc5fdd137f720e8c47618e5ea9e9ab9803cc8d50238a88261af94ebca082e8a5920b4a98356fc942b19b5091efa0a09323f4cf1b9f456f52cd19bb1c95fd1ecaae8548a51427fa0eeb7555204318da248ad429ac73a797749069681f349eda3941f216c74bbbae5f0dc77ffcc00e714639d7d").unwrap();
        let initializer = StmInitializer::setup_new(params, &cbor_sk_bytes, 0, 10, &mut rng);

        let cbor_bytes = Vec::from_hex("8284582067fd5ccf770c0182a34d2b3d2011ca3a853ba947e17cae7543e668bc7687eb6a0000584050592bef1c630f2df499161d78bfadb44cc76cfd24048993ace4a45dade37b4f29e95172fde4e63581a93552f6986985616b70f61062a1db2ee0d3d8e671440e58202abf3ff537a2080f53fa38615906fa6094d44860902f2b2dffdbb41b811ff39f").expect("Invalid Hex String");
        assert!(key_reg
            .register(&cbor_bytes, initializer.kes_sig.unwrap(), 0, initializer.pk)
            .is_ok());
    }
}
