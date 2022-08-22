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
pub type PoolId = [u8; 32];

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

        let mut pool_id = [0u8; 32];
        pool_id.copy_from_slice(&blake2::Blake2b::digest(cert.cold_vk.as_bytes()).as_slice()[..32]);

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
<<<<<<< HEAD
    use blake2::{digest::consts::U32, Blake2b};
=======
    use blake2::Blake2b;
    use hex::FromHex;
>>>>>>> a8dd20439 (Serialisation and verification of OpCert)
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
}
