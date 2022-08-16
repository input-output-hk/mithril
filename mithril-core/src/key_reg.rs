//! Key registration functionality.

use crate::error::RegisterError;
use digest::{Digest, FixedOutput};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

#[cfg(not(feature = "zcash"))]
use crate::multi_sig::{VerificationKey, VerificationKeyPoP};
#[cfg(feature = "zcash")]
use crate::multi_sig_zcash::{VerificationKey, VerificationKeyPoP};

use super::stm::Stake;
use crate::merkle_tree::{MTLeaf, MerkleTree};

/// Stores a registered party with its public key and the associated stake.
pub type RegParty = MTLeaf;

/// Struct that collects public keys and stakes of parties.
/// Each participant (both the signers and the clerks) need to run their own instance of the key registration.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KeyReg {
    keys: HashMap<VerificationKey, Stake>,
}

/// Structure generated out of a closed registration containing the registered parties, total stake, and the merkle tree.
/// One can only get a global `avk` out of a closed key registration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosedKeyReg<D: Digest + FixedOutput> {
    /// Ordered list of registered parties.
    pub reg_parties: Vec<RegParty>,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub merkle_tree: Arc<MerkleTree<D>>,
}

impl KeyReg {
    /// Initialise an empty `KeyReg`.
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
    pub fn close<D: Digest + FixedOutput>(self) -> ClosedKeyReg<D> {
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
    #[cfg(not(feature = "zcash"))]
    use crate::multi_sig::SigningKey;
    #[cfg(feature = "zcash")]
    use crate::multi_sig_zcash::SigningKey;
    use blake2::Blake2b;
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
                }
            }

            if !kr.keys.is_empty() {
                let closed = kr.close::<Blake2b>();
                let retrieved_keys = closed.reg_parties.iter().map(|r| (r.0, r.1)).collect::<HashMap<_,_>>();
                assert!(retrieved_keys == keys);
            }
        }
    }
}

// use std::path::Path;
// use std::io::prelude::*;
// use std::fs::File;
// use rand::rngs::OsRng;
// use std::io;
//
// #[test]
// fn mock_genesis_key () -> io::Result<()>{
//
//     extern crate rand;
//     extern crate ed25519_dalek;
//
//     use ed25519_dalek::{Keypair, PublicKey, SecretKey, SignatureError};
//
//     let mut csprng = OsRng{};
//     let keypair: Keypair = Keypair::generate(&mut csprng);
//     let public_key: PublicKey = keypair.public;
//     let secret_key: SecretKey = keypair.secret;
//
//
//     let pathv = Path::new("../TEST_ONLY_genesis.vkey");
//     let displayv = pathv.display();
//
//     let mut filev = match File::create(&pathv) {
//         Err(why) => panic!("couldn't create {}: {}", displayv, why),
//         Ok(filev) => filev,
//     };
//
//     match filev.write_all(&public_key.to_bytes()) {
//         Err(why) => panic!("Couldn't write to {}: {}", displayv, why),
//         Ok(_) => println!("Content successfully written to {}.", displayv),
//     }
//
//     let paths = Path::new("TEST_ONLY_genesis.skey");
//     let displays = paths.display();
//
//     let mut files = match File::create(&paths) {
//         Err(why) => panic!("couldn't create {}: {}", displays, why),
//         Ok(files) => files,
//     };
//
//     match files.write_all(&secret_key.to_bytes()) {
//         Err(why) => panic!("Couldn't write to {}: {}", displays, why),
//         Ok(_) => println!("Content successfully written to {}.", displays),
//     }
//
//
//     let mut fs = File::open("TEST_ONLY_genesis.skey")?;
//     let mut buffers = Vec::new();
//     // read the whole file
//     fs.read_to_end(&mut buffers)?;
//
//     let sk : Result<SecretKey, SignatureError> = SecretKey::from_bytes(&buffers);
//
//     let pks: PublicKey = (&sk.unwrap()).into();
//
//
//     let mut fv = File::open("../TEST_ONLY_genesis.vkey")?;
//     let mut bufferv = Vec::new();
//     // read the whole file
//     fv.read_to_end(&mut bufferv)?;
//
//     let pk : Result<PublicKey, SignatureError> = PublicKey::from_bytes(&bufferv);
//
//     if public_key.eq(&pk.unwrap()) {
//         println!("pk");
//     }
//     if public_key.eq(&pks) {
//         println!("pks");
//     }
//
//     Ok(())
// }