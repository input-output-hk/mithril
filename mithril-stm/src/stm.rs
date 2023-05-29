//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.
//! See figure 6 of [the paper](https://eprint.iacr.org/2021/916) for most of the
//! protocol.
//!
//! What follows is a simple example showing the usage of STM.
//!
//! ```rust
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use blake2::{Blake2b, digest::consts::U32};
//! use mithril_stm::key_reg::KeyReg; // Import key registration functionality
//! use mithril_stm::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSignerAvk};
//! use mithril_stm::AggregationError;
//! use rayon::prelude::*; // We use par_iter to speed things up
//!
//! use rand_chacha::ChaCha20Rng;
//! use rand_core::{RngCore, SeedableRng};
//!
//! let nparties = 4; // Use a small number of parties for this example
//! type D = Blake2b<U32>; // Setting the hash function for convenience
//!
//! let mut rng = ChaCha20Rng::from_seed([0u8; 32]); // create and initialize rng
//! let mut msg = [0u8; 16]; // setting an arbitrary message
//! rng.fill_bytes(&mut msg);
//!
//! // In the following, we will have 4 parties try to sign `msg`, then aggregate and
//! // verify those signatures.
//!
//! //////////////////////////
//! // initialization phase //
//! //////////////////////////
//!
//! // Set low parameters for testing
//! // XXX: not for production
//! let params = StmParameters {
//!     m: 100, // Security parameter XXX: not for production
//!     k: 2, // Quorum parameter XXX: not for production
//!     phi_f: 0.2, // Lottery parameter XXX: not for production
//! };
//!
//! // Generate some arbitrary stake for each party
//! // Stake is an integer.
//! // Total stake of all parties is total stake in the system.
//! let stakes = (0..nparties)
//!     .into_iter()
//!     .map(|_| 1 + (rng.next_u64() % 9999))
//!     .collect::<Vec<_>>();
//!
//! // Create a new key registry from the parties and their stake
//! let mut key_reg = KeyReg::init();
//!
//! // For each party, crate a StmInitializer.
//! // This struct can create keys for the party.
//! let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties);
//! for stake in stakes {
//!     // Create keys for this party
//!     let p = StmInitializer::setup(params, stake, &mut rng);
//!     // Register keys with the KeyReg service
//!     key_reg
//!         .register(p.stake, p.verification_key())
//!         .unwrap();
//!     ps.push(p);
//! }
//!
//! // Close the key registration.
//! let closed_reg = key_reg.close();
//!
//! // Finalize the StmInitializer and turn it into a StmSigner, which can execute the
//! // rest of the protocol.
//! let ps = ps
//!     .into_par_iter()
//!     .map(|p| p.new_signer_avk(closed_reg.clone()).unwrap())
//!     .collect::<Vec<StmSignerAvk<D>>>();
//!
//! /////////////////////
//! // operation phase //
//! /////////////////////
//!
//! // Next, each party tries to sign the message for each index available.
//! // We collect the successful signatures into a vec.
//! let sigs = ps
//!     .par_iter()
//!     .filter_map(|p| {
//!         return p.sign(&msg);
//!     })
//!     .collect::<Vec<StmSig>>();
//!
//! // StmClerk can aggregate and verify signatures.
//! let clerk = StmClerk::from_signer(&ps[0]);
//!
//! // Aggregate and verify the signatures
//! let msig = clerk.aggregate(&sigs, &msg);
//! match msig {
//!     Ok(aggr) => {
//!         println!("Aggregate ok");
//!         assert!(aggr
//!             .verify(&msg, &clerk.compute_avk(), &params)
//!             .is_ok());
//!     }
//!     Err(AggregationError::NotEnoughSignatures(n, k)) => {
//!         println!("Not enough signatures");
//!         assert!(n < params.k && k == params.k)
//!     }
//!     Err(_) => unreachable!(),
//! }
//! # Ok(())
//! # }
//! ```

use crate::eligibility_check::ev_lt_phi;
use crate::error::{
    AggregationError, RegisterError, StmAggregateSignatureError, StmSignatureError,
};
use crate::key_reg::{ClosedKeyReg, RegParty};
use crate::merkle_tree::{BatchPath, MTLeaf, MerkleTreeCommitmentBatchCompat};
use crate::multi_sig::{Signature, SigningKey, VerificationKey, VerificationKeyPoP};
use blake2::digest::{Digest, FixedOutput};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::{From, TryFrom, TryInto};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// Wrapper of the MultiSignature Verification key with proof of possession
pub type StmVerificationKeyPoP = VerificationKeyPoP;

/// Wrapper of the MultiSignature Verification key
pub type StmVerificationKey = VerificationKey;

/// Used to set protocol parameters.
// todo: this is the criteria to consider parameters valid:
// Let A = max assumed adversarial stake
// Let a = A / max_stake
// Let p = φ(a)  // f needs tuning, something close to 0.2 is reasonable
// Then, we're secure if SUM[from i=k to i=m] Binomial(i successes, m experiments, p chance of success) <= 2^-100 or thereabouts.
// The latter turns to 1 - BinomialCDF(k-1,m,p)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct StmParameters {
    /// Security parameter, upper bound on indices.
    pub m: u64,
    /// Quorum parameter.
    pub k: u64,
    /// `f` in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant..
    pub phi_f: f64,
}

/// Initializer for `StmSigner`.
/// This is the data that is used during the key registration procedure.
/// Once the latter is finished, this instance is consumed into an `StmSigner`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializer {
    /// This participant's stake.
    pub stake: Stake,
    /// Current protocol instantiation parameters.
    pub params: StmParameters,
    /// Secret key.
    pub(crate) sk: SigningKey,
    /// Verification (public) key + proof of possession.
    pub(crate) pk: StmVerificationKeyPoP,
}

/// Participant in the protocol can sign messages.
/// This instance can only be generated out of an `StmInitializer` and a `ClosedKeyReg`.
/// This ensures that a `MerkleTree` root is not computed before all participants have registered.
#[derive(Debug, Clone)]
pub struct StmSigner {
    mt_index: u64,
    stake: Stake,
    params: StmParameters,
    sk: SigningKey,
    vk: StmVerificationKey,
}

/// Participant in the protocol can sign messages.
/// This instance can only be generated out of an `StmInitializer` and a `ClosedKeyReg`.
/// This ensures that a `MerkleTree` root is not computed before all participants have registered.
#[derive(Debug, Clone)]
pub struct StmSignerAvk<D: Digest> {
    stm_signer: StmSigner,
    closed_reg: ClosedKeyReg<D>,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct StmClerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyReg<D>,
    pub(crate) params: StmParameters,
}

/// Signature created by a single party who has won the lottery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmSig {
    /// The signature from the underlying MSP scheme.
    pub sigma: Signature,
    /// The index(es) for which the signature is valid
    pub indexes: Vec<Index>,
    /// Merkle tree index of the signer.
    pub signer_index: Index,
}

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "BatchPath<D>: Serialize",
    deserialize = "BatchPath<D>: Deserialize<'de>"
))]
pub struct StmAggrVerificationKey<D: Clone + Digest + FixedOutput> {
    mt_commitment: MerkleTreeCommitmentBatchCompat<D>,
    total_stake: Stake,
}

/// `StmMultiSig` uses the "concatenation" proving system (as described in Section 4.3 of the original paper.)
/// This means that the aggregated signature contains a vector with all individual signatures.
/// BatchPath is also a part of the aggregate signature which covers path for all signatures.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "BatchPath<D>: Serialize",
    deserialize = "BatchPath<D>: Deserialize<'de>"
))]
pub struct StmAggrSig<D: Clone + Digest + FixedOutput> {
    pub(crate) signatures: Vec<(StmSig, RegParty)>,
    /// The list of unique merkle tree nodes that covers path for all signatures.
    pub batch_proof: BatchPath<D>,
}

/// Full node verifier
pub struct FullNodeVerifier<D: Digest> {
    /// Ordered list of registered parties.
    pub eligible_parties: Vec<RegParty>,
    /// Total stake of registered parties.
    pub total_stake: Stake,
    hasher: PhantomData<D>,
}

impl StmParameters {
    /// Convert to bytes
    /// # Layout
    /// * Security parameter, `m` (as u64)
    /// * Quorum parameter, `k` (as u64)
    /// * Phi f, as (f64)
    pub fn to_bytes(&self) -> [u8; 24] {
        let mut out = [0; 24];
        out[..8].copy_from_slice(&self.m.to_be_bytes());
        out[8..16].copy_from_slice(&self.k.to_be_bytes());
        out[16..].copy_from_slice(&self.phi_f.to_be_bytes());
        out
    }

    /// Extract the `StmParameters` from a byte slice.
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
        if bytes.len() != 24 {
            return Err(RegisterError::SerializationError);
        }

        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let m = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let k = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[16..]);
        let phi_f = f64::from_be_bytes(u64_bytes);

        Ok(Self { m, k, phi_f })
    }
}

impl StmInitializer {
    /// Builds an `StmInitializer` that is ready to register with the key registration service.
    /// This function generates the signing and verification key with a PoP, and initialises the structure.
    pub fn setup<R: RngCore + CryptoRng>(params: StmParameters, stake: Stake, rng: &mut R) -> Self {
        let sk = SigningKey::gen(rng);
        let pk = StmVerificationKeyPoP::from(&sk);
        Self {
            stake,
            params,
            sk,
            pk,
        }
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKeyPoP {
        self.pk
    }

    /// Function that checks whether the initializer is registered.
    pub fn check_initializer<D: Digest + Clone>(
        &self,
        closed_reg: ClosedKeyReg<D>,
    ) -> Result<Option<u64>, RegisterError> {
        let mut my_index = None;
        for (i, rp) in closed_reg.reg_parties.iter().enumerate() {
            if rp.0 == self.pk.vk {
                my_index = Some(i as u64);
                break;
            }
        }
        if my_index.is_none() {
            return Err(RegisterError::UnregisteredInitializer);
        }
        Ok(my_index)
    }

    /// Build the `avk` for the given list of parties.
    ///
    /// Note that if this StmInitializer was modified *between* the last call to `register`,
    /// then the resulting `StmSigner` may not be able to produce valid signatures.
    ///
    /// Returns an `StmSigner` specialized to
    /// * this `StmSigner`'s ID and current stake
    /// * this `StmSigner`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    pub fn new_signer(self, index: Index) -> StmSigner {
        StmSigner {
            mt_index: index,
            stake: self.stake,
            params: self.params,
            sk: self.sk,
            vk: self.pk.vk,
        }
    }

    /// Build the `avk` for the given list of parties.
    ///
    /// Note that if this StmInitializer was modified *between* the last call to `register`,
    /// then the resulting `StmSigner` may not be able to produce valid signatures.
    ///
    /// Returns an `StmSigner` specialized to
    /// * this `StmSigner`'s ID and current stake
    /// * this `StmSigner`'s parameter valuation
    /// * the `avk` as built from the current registered parties (according to the registration service)
    /// * the current total stake (according to the registration service)
    /// # Error
    /// This function fails if the initializer is not registered.
    pub fn new_signer_avk<D: Digest + Clone>(
        self,
        closed_reg: ClosedKeyReg<D>,
    ) -> Result<StmSignerAvk<D>, RegisterError> {
        let my_index = self.check_initializer(closed_reg.clone())?;
        let stm_signer = self.new_signer(my_index.unwrap());
        Ok(StmSignerAvk {
            stm_signer,
            closed_reg,
        })
    }

    /// Convert to bytes
    /// # Layout
    /// * Stake (u64)
    /// * Params
    /// * Secret Key
    /// * Public key (including PoP)
    pub fn to_bytes(&self) -> [u8; 256] {
        let mut out = [0u8; 256];
        out[..8].copy_from_slice(&self.stake.to_be_bytes());
        out[8..32].copy_from_slice(&self.params.to_bytes());
        out[32..64].copy_from_slice(&self.sk.to_bytes());
        out[64..].copy_from_slice(&self.pk.to_bytes());
        out
    }

    /// Convert a slice of bytes to an `StmInitializer`
    /// # Error
    /// The function fails if the given string of bytes is not of required size.
    pub fn from_bytes(bytes: &[u8]) -> Result<StmInitializer, RegisterError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let stake = u64::from_be_bytes(u64_bytes);
        let params = StmParameters::from_bytes(&bytes[8..32])?;
        let sk = SigningKey::from_bytes(&bytes[32..])?;
        let pk = StmVerificationKeyPoP::from_bytes(&bytes[64..])?;

        Ok(Self {
            stake,
            params,
            sk,
            pk,
        })
    }
}

impl StmSigner {
    /// This function produces a signature following the description of Section 2.4.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    /// If it wins at least one lottery, it stores the signer's merkle tree index. The proof of membership
    /// will be handled by the aggregator.
    pub fn sign(&self, msg: &[u8], total_stake: Stake) -> Option<StmSig> {
        let sigma = self.sk.sign(msg);

        let indexes = self.check_lottery(msg, &sigma, total_stake);

        if !indexes.is_empty() {
            Some(StmSig {
                sigma,
                indexes,
                signer_index: self.mt_index,
            })
        } else {
            None
        }
    }

    /// Checks whether the indices won the lottery.
    pub fn check_lottery(&self, msg: &[u8], sigma: &Signature, total_stake: Stake) -> Vec<u64> {
        let mut indexes = Vec::new();
        for index in 0..self.params.m {
            if ev_lt_phi(
                self.params.phi_f,
                sigma.eval(msg, index),
                self.stake,
                total_stake,
            ) {
                indexes.push(index);
            }
        }
        indexes
    }
}

impl<D: Clone + Digest + FixedOutput> StmSignerAvk<D> {
    /// This function produces a signature following the description of Section 2.4.
    /// Once the signature is produced, this function checks whether any index in `[0,..,self.params.m]`
    /// wins the lottery by evaluating the dense mapping.
    /// It records all the winning indexes in `Self.indexes`.
    /// If it wins at least one lottery, it stores the signer's merkle tree index. The proof of membership
    /// will be handled by the aggregator.
    pub fn sign(&self, msg: &[u8]) -> Option<StmSig> {
        let msgp = self
            .closed_reg
            .merkle_tree
            .to_commitment_batch_compat()
            .concat_with_msg(msg);
        let signature = StmSigner::sign(&self.stm_signer, &msgp, self.closed_reg.total_stake)?;

        if !signature.indexes.is_empty() {
            Some(StmSig {
                sigma: signature.sigma,
                indexes: signature.indexes,
                signer_index: self.stm_signer.mt_index,
            })
        } else {
            None
        }
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration, which consists of
    /// the merkle tree root and the total stake.
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        StmAggrVerificationKey::from(&self.closed_reg)
    }

    /// Return the closed registration instance
    pub fn get_closed_reg(self) -> ClosedKeyReg<D> {
        self.closed_reg
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKey {
        self.stm_signer.vk
    }

    /// Extract stake from the signer.
    pub fn get_stake(&self) -> Stake {
        self.stm_signer.stake
    }
}

impl<D: Digest + Clone + FixedOutput> StmClerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn from_registration(params: &StmParameters, closed_reg: &ClosedKeyReg<D>) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a Clerk from a signer.
    pub fn from_signer(signer: &StmSignerAvk<D>) -> Self {
        Self {
            params: signer.stm_signer.params,
            closed_reg: signer.closed_reg.clone(),
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `StmAggrSig`.
    pub fn aggregate(
        &self,
        sigs: &[StmSig],
        msg: &[u8],
    ) -> Result<StmAggrSig<D>, AggregationError> {
        let mut unique_sigs = self.dedup_sigs_for_indices(msg, sigs)?;
        unique_sigs.sort_unstable();
        let signatures = unique_sigs
            .iter()
            .map(|sig| {
                (
                    sig.clone(),
                    self.closed_reg.reg_parties[sig.signer_index as usize],
                )
            })
            .collect(); // todo: look into this conversion

        let mt_index_list = unique_sigs
            .iter()
            .map(|sig| sig.signer_index as usize)
            .collect::<Vec<usize>>();

        let batch_proof = self.closed_reg.merkle_tree.get_batched_path(mt_index_list);

        Ok(StmAggrSig {
            signatures,
            batch_proof,
        })
    }

    /// Given a slice of `sigs`, this function returns a new list of signatures with only valid indices.
    /// In case of conflict (having several signatures for the same index)
    /// it selects the smallest signature (i.e. takes the signature with the smallest scalar).
    /// The function selects at least `self.k` indexes.
    ///  # Error
    /// If there is no sufficient signatures, then the function fails.
    // todo: We need to agree on a criteria to dedup (by defaut we use a BTreeMap that guarantees keys order)
    // todo: not good, because it only removes index if there is a conflict (see benches)
    pub fn dedup_sigs_for_indices(
        &self,
        msg: &[u8],
        sigs: &[StmSig],
    ) -> Result<Vec<StmSig>, AggregationError> {
        let avk = StmAggrVerificationKey::from(&self.closed_reg);
        let mut sig_by_index: BTreeMap<Index, &StmSig> = BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&StmSig, Vec<Index>> = HashMap::new();
        let reg_parties = sigs
            .iter()
            .map(|sig| self.closed_reg.reg_parties[sig.signer_index as usize])
            .collect::<Vec<RegParty>>(); // todo: look into this

        for (sig, reg_party) in sigs.iter().zip(reg_parties.iter()) {
            if sig
                .verify_avk(&self.params, &reg_party.0, &reg_party.1, &avk, msg)
                .is_err()
            {
                continue;
            }

            for index in sig.indexes.iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_sig) = sig_by_index.get(index) {
                    let sig_to_remove_index = if sig.sigma < previous_sig.sigma {
                        insert_this_sig = true;
                        previous_sig
                    } else {
                        sig
                    };

                    if let Some(indexes) = removal_idx_by_vk.get_mut(sig_to_remove_index) {
                        indexes.push(*index);
                    } else {
                        removal_idx_by_vk.insert(sig_to_remove_index, vec![*index]);
                    }
                } else {
                    insert_this_sig = true;
                }

                if insert_this_sig {
                    sig_by_index.insert(*index, sig);
                }
            }
        }

        let mut dedup_sigs: HashSet<StmSig> = HashSet::new();
        let mut count: u64 = 0;

        for (_, &sig) in sig_by_index.iter() {
            if dedup_sigs.contains(sig) {
                continue;
            }
            let mut deduped_sig = sig.clone();
            if let Some(indexes) = removal_idx_by_vk.get(sig) {
                deduped_sig.indexes = deduped_sig
                    .indexes
                    .clone()
                    .into_iter()
                    .filter(|i| !indexes.contains(i))
                    .collect();
            }

            let size: Result<u64, _> = deduped_sig.indexes.len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_sig) {
                    panic!("Should not reach!");
                }
                dedup_sigs.insert(deduped_sig);
                count += size;

                if count >= self.params.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }

        Err(AggregationError::NotEnoughSignatures(count, self.params.k))
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration.
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        StmAggrVerificationKey::from(&self.closed_reg)
    }

    /// Get the (VK, stake) of a party given its index.
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(StmVerificationKey, Stake)> {
        self.closed_reg
            .reg_parties
            .get(*party_index as usize)
            .map(|&r| r.into())
    }
}

impl StmSig {
    /// Verify an stm signature by checking that the lottery was won, the merkle path is correct,
    /// the indexes are in the desired range and the underlying multi signature validates.
    pub fn verify_avk<D: Clone + Digest + FixedOutput>(
        &self,
        params: &StmParameters,
        pk: &StmVerificationKey,
        stake: &Stake,
        avk: &StmAggrVerificationKey<D>,
        msg: &[u8],
    ) -> Result<(), StmSignatureError> {
        let msgp = avk.mt_commitment.concat_with_msg(msg);
        self.verify(params, pk, stake, &msgp, &avk.total_stake)?;
        Ok(())
    }

    /// Verify that all indices of a signature are valid.
    pub(crate) fn check_indices(
        &self,
        params: &StmParameters,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> Result<(), StmSignatureError> {
        for &index in &self.indexes {
            if index > params.m {
                return Err(StmSignatureError::IndexBoundFailed(index, params.m));
            }

            let ev = self.sigma.eval(msg, index);

            if !ev_lt_phi(params.phi_f, ev, *stake, *total_stake) {
                return Err(StmSignatureError::LotteryLost);
            }
        }

        Ok(())
    }

    /// Convert an `StmSig` into bytes
    ///
    /// # Layout
    /// * Stake
    /// * Number of valid indexes (as u64)
    /// * Indexes of the signature
    /// * Public Key
    /// * Signature
    /// * Merkle index of the signer.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&(self.indexes.len() as u64).to_be_bytes());

        for index in &self.indexes {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.sigma.to_bytes());

        output.extend_from_slice(&self.signer_index.to_be_bytes());
        output
    }

    /// Extract a batch compatible `StmSig` from a byte slice.
    pub fn from_bytes<D: Clone + Digest + FixedOutput>(
        bytes: &[u8],
    ) -> Result<StmSig, StmSignatureError> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(&bytes[0..8]);
        let nr_indexes = u64::from_be_bytes(u64_bytes) as usize;

        let mut indexes = Vec::new();
        for i in 0..nr_indexes {
            u64_bytes.copy_from_slice(&bytes[8 + i * 8..16 + i * 8]);
            indexes.push(u64::from_be_bytes(u64_bytes));
        }

        let offset = 8 + nr_indexes * 8;
        let sigma = Signature::from_bytes(&bytes[offset..offset + 48])?;

        u64_bytes.copy_from_slice(&bytes[offset + 48..offset + 56]);
        let signer_index = u64::from_be_bytes(u64_bytes);

        Ok(StmSig {
            sigma,
            indexes,
            signer_index,
        })
    }

    /// Compare two `StmSig` by their signers' merkle tree indexes.
    pub fn cmp_stm_sig(&self, other: &Self) -> Ordering {
        self.signer_index.cmp(&other.signer_index)
    }

    /// Verify full node signature
    pub fn verify(
        &self,
        params: &StmParameters,
        pk: &StmVerificationKey,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> Result<(), StmSignatureError> {
        self.sigma.verify(msg, pk)?;
        self.check_indices(params, stake, msg, total_stake)?;

        Ok(())
    }
}

impl Hash for StmSig {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.sigma.to_bytes(), state)
    }
}

impl PartialEq for StmSig {
    fn eq(&self, other: &Self) -> bool {
        self.sigma == other.sigma
    }
}

impl Eq for StmSig {}

impl PartialOrd for StmSig {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_stm_sig(other))
    }
}

impl Ord for StmSig {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_stm_sig(other)
    }
}

impl<D: Clone + Digest + FixedOutput> From<&ClosedKeyReg<D>> for StmAggrVerificationKey<D> {
    fn from(reg: &ClosedKeyReg<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_commitment_batch_compat(),
            total_stake: reg.total_stake,
        }
    }
}

impl<D: Clone + Digest + FixedOutput + Send + Sync> StmAggrSig<D> {
    /// Verify all checks from signatures, except for the signature verification itself.
    fn preliminary_verify(
        &self,
        msg: &[u8],
        avk: &StmAggrVerificationKey<D>,
        parameters: &StmParameters,
    ) -> Result<(Vec<Signature>, Vec<VerificationKey>), StmAggregateSignatureError<D>> {
        let msgp = avk.mt_commitment.concat_with_msg(msg);

        let leaves = self
            .signatures
            .iter()
            .map(|r| r.1)
            .collect::<Vec<RegParty>>();

        let signatures = self
            .signatures
            .iter()
            .map(|r| r.0.clone())
            .collect::<Vec<StmSig>>();

        FullNodeVerifier::pre_verify(
            &avk.total_stake,
            signatures.as_slice(),
            parameters,
            msgp.as_slice(),
            leaves.clone(),
        )?;

        let proof = &self.batch_proof;

        avk.mt_commitment.check(&leaves, &proof.clone())?;

        let (sigs, vks) = FullNodeVerifier::<D>::collect_ver_data(signatures.as_slice(), &leaves);
        Ok((sigs, vks))
    }

    /// Verify aggregate signature, by checking that
    /// * each signature contains only valid indices,
    /// * the lottery is indeed won by each one of them,
    /// * the merkle tree path is valid,
    /// * the aggregate signature validates with respect to the aggregate verification key
    /// (aggregation is computed using functions `MSP.BKey` and `MSP.BSig` as described in Section 2.4 of the paper).
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &StmAggrVerificationKey<D>,
        parameters: &StmParameters,
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let msgp = avk.mt_commitment.concat_with_msg(msg);
        let (sigs, vks) = self.preliminary_verify(msg, avk, parameters)?;

        Signature::verify_aggregate(msgp.as_slice(), &vks, &sigs)?;
        Ok(())
    }

    /// Batch verify a set of signatures, with different messages and avks.
    pub fn batch_verify(
        stm_signatures: &[Self],
        msgs: &[Vec<u8>],
        avks: &[StmAggrVerificationKey<D>],
        parameters: &[StmParameters],
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let batch_size = stm_signatures.len();
        assert_eq!(
            batch_size,
            msgs.len(),
            "Number of messages should correspond to size of the batch"
        );
        assert_eq!(
            batch_size,
            avks.len(),
            "Number of avks should correspond to size of the batch"
        );
        assert_eq!(
            batch_size,
            parameters.len(),
            "Number of parameters should correspond to size of the batch"
        );

        let mut aggr_sigs = Vec::with_capacity(batch_size);
        let mut aggr_vks = Vec::with_capacity(batch_size);
        for (idx, sig_group) in stm_signatures.iter().enumerate() {
            sig_group.preliminary_verify(&msgs[idx], &avks[idx], &parameters[idx])?;
            let grouped_sigs: Vec<Signature> = sig_group
                .signatures
                .iter()
                .map(|(sig, _)| sig.sigma)
                .collect();
            let grouped_vks: Vec<VerificationKey> = sig_group
                .signatures
                .iter()
                .map(|(_, reg_party)| reg_party.0)
                .collect();

            let (aggr_vk, aggr_sig) = Signature::aggregate(&grouped_vks, &grouped_sigs).unwrap();
            aggr_sigs.push(aggr_sig);
            aggr_vks.push(aggr_vk);
        }

        let concat_msgs: Vec<Vec<u8>> = msgs
            .iter()
            .zip(avks.iter())
            .map(|(msg, avk)| avk.mt_commitment.concat_with_msg(msg))
            .collect();

        Signature::batch_verify_aggregates(&concat_msgs, &aggr_vks, &aggr_sigs)?;
        Ok(())
    }

    /// Convert multi signature to bytes
    /// # Layout
    /// * Number of signatures (as u64)
    /// * Size of a signature
    /// * Pairs of Signatures and Registered Parties
    /// * Batch proof
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&u64::try_from(self.signatures.len()).unwrap().to_be_bytes());
        out.extend_from_slice(
            &u64::try_from(self.signatures[0].0.to_bytes().len())
                .unwrap()
                .to_be_bytes(),
        );
        for (sig, reg_party) in &self.signatures {
            out.extend_from_slice(&sig.to_bytes());
            out.extend_from_slice(&reg_party.to_bytes());
        }
        let proof = &self.batch_proof;
        out.extend_from_slice(&proof.to_bytes());

        out
    }

    ///Extract a `StmAggrSig` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<StmAggrSig<D>, StmAggregateSignatureError<D>> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(&bytes[..8]);
        let size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| StmAggregateSignatureError::SerializationError)?;

        u64_bytes.copy_from_slice(&bytes[8..16]);
        let sig_size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| StmAggregateSignatureError::SerializationError)?;

        let mut signatures = Vec::with_capacity(size);
        for i in 0..size {
            signatures.push((
                StmSig::from_bytes::<D>(
                    &bytes[16 + i * (sig_size + 104)..16 + sig_size + i * (sig_size + 104)],
                )?,
                MTLeaf::from_bytes(
                    &bytes[16 + sig_size + i * (sig_size + 104)..16 + (i + 1) * (sig_size + 104)],
                )
                .map_err(|_| StmAggregateSignatureError::SerializationError)?,
            ));
        }

        let offset = 16 + (sig_size + 104) * size;
        let batch_proof = BatchPath::from_bytes(&bytes[offset..])?;

        Ok(StmAggrSig {
            signatures,
            batch_proof,
        })
    }
}

impl<D: Digest + FixedOutput> FullNodeVerifier<D> {
    /// Verification for a full node verifier
    pub fn pre_verify(
        total_stake: &Stake,
        signatures: &[StmSig],
        parameters: &StmParameters,
        msg: &[u8],
        signed_parties: Vec<RegParty>,
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let mut nr_indices = 0;
        let mut unique_indices = HashSet::new();

        for (i, sig) in signatures.iter().enumerate() {
            sig.check_indices(parameters, &signed_parties[i].1, msg, total_stake)?;
            for &index in &sig.indexes {
                unique_indices.insert(index);
                nr_indices += 1;
            }
        }

        if nr_indices != unique_indices.len() {
            return Err(StmAggregateSignatureError::IndexNotUnique);
        }

        if (nr_indices as u64) < parameters.k {
            return Err(StmAggregateSignatureError::NoQuorum);
        }

        Ok(())
    }

    /// Collect signatures and verification keys
    pub fn collect_ver_data(
        signatures: &[StmSig],
        signed_parties: &[RegParty],
    ) -> (Vec<Signature>, Vec<VerificationKey>) {
        let sigs = signatures
            .iter()
            .map(|sig| sig.sigma)
            .collect::<Vec<Signature>>();
        let vks = signed_parties
            .iter()
            .map(|reg_party| reg_party.0)
            .collect::<Vec<VerificationKey>>();

        (sigs, vks)
    }

    /// Verify
    pub fn verify(
        &self,
        signatures: &[StmSig],
        parameters: &StmParameters,
        msg: &[u8],
    ) -> Result<(), StmAggregateSignatureError<D>> {
        let signed_parties = self.collect_signed_parties(signatures);

        Self::pre_verify(
            &self.total_stake,
            signatures,
            parameters,
            msg,
            signed_parties.clone(),
        )?;

        let (sigs, vks) = Self::collect_ver_data(signatures, &signed_parties);

        Signature::verify_aggregate(msg.to_vec().as_slice(), &vks, &sigs)?;

        Ok(())
    }

    /// Collect signed parties
    pub fn collect_signed_parties(&self, signatures: &[StmSig]) -> Vec<RegParty> {
        let indices = signatures
            .iter()
            .map(|sig| sig.signer_index as usize)
            .collect::<Vec<usize>>();
        let signed_parties = self
            .eligible_parties
            .iter()
            .enumerate()
            .filter_map(|(ind, party)| {
                if indices.contains(&ind) {
                    Some(*party)
                } else {
                    None
                }
            })
            .collect();
        signed_parties
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key_reg::*;
    use crate::merkle_tree::BatchPath;
    use bincode;
    use blake2::{digest::consts::U32, Blake2b};
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use proptest::test_runner::{RngAlgorithm::ChaCha, TestRng};
    use std::collections::{HashMap, HashSet};

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type Sig = StmAggrSig<D>;
    type D = Blake2b<U32>;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSignerAvk<D>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSignerAvk<D>> {
        let mut kr = KeyReg::init();
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = ChaCha20Rng::from_seed(trng.gen());

        #[allow(clippy::needless_collect)]
        let ps = stake
            .into_iter()
            .map(|stake| {
                let p = StmInitializer::setup(params, stake, &mut rng);
                kr.register(stake, p.pk).unwrap();
                p
            })
            .collect::<Vec<_>>();
        let closed_reg = kr.close();
        ps.into_iter()
            .map(|p| p.new_signer_avk(closed_reg.clone()).unwrap())
            .collect()
    }

    /// Generate a vector of stakes that should sum to `honest_stake`
    /// when ignoring the indices in `adversaries`
    fn arb_honest_for_adversaries(
        num_parties: usize,
        honest_stake: Stake,
        adversaries: HashMap<usize, Stake>,
    ) -> impl Strategy<Value = Vec<Stake>> {
        vec(1..honest_stake, num_parties).prop_map(move |parties| {
            let honest_sum = parties.iter().enumerate().fold(0, |acc, (i, s)| {
                if !adversaries.contains_key(&i) {
                    acc + s
                } else {
                    acc
                }
            });

            parties
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if let Some(a) = adversaries.get(&i) {
                        *a
                    } else {
                        (*s * honest_stake) / honest_sum
                    }
                })
                .collect()
        })
    }

    /// Generate a vector of `num_parties` stakes summing to `num_parties * total_stake`,
    /// plus a subset S of 0..num_parties such that the sum of the stakes at indices
    /// in S is adversary_stake * N
    fn arb_parties_with_adversaries(
        num_parties: usize,
        num_adversaries: usize,
        total_stake: Stake,
        adversary_stake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        hash_map(0..num_parties, 1..total_stake, num_adversaries).prop_flat_map(
            move |adversaries| {
                let adversary_sum: Stake = adversaries.values().sum();
                let adversaries_normed = adversaries
                    .iter()
                    .map(|(a, stake)| (*a, (stake * adversary_stake) / adversary_sum))
                    .collect();

                let adversaries = adversaries.into_keys().collect();
                (
                    Just(adversaries),
                    arb_honest_for_adversaries(
                        num_parties,
                        total_stake - adversary_stake,
                        adversaries_normed,
                    ),
                )
            },
        )
    }

    fn find_signatures(msg: &[u8], ps: &[StmSignerAvk<D>], is: &[usize]) -> Vec<StmSig> {
        let mut sigs = Vec::new();
        for i in is {
            if let Some(sig) = ps[*i].sign(msg) {
                sigs.push(sig);
            }
        }
        sigs
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that `dedup_sigs_for_indices` only takes valid signatures.
        fn test_dedup(msg in any::<[u8; 16]>()) {
            let false_msg = [1u8; 20];
            let params = StmParameters { m: 1, k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, 1);
            let clerk = StmClerk::from_signer(&ps[0]);
            let avk = clerk.compute_avk();
            let mut sigs = Vec::with_capacity(2);


            if let Some(sig) = ps[0].sign(&false_msg) {
                sigs.push(sig);
            }

            if let Some(sig) = ps[0].sign(&msg) {
                sigs.push(sig);
            }

            let dedup_result = clerk.dedup_sigs_for_indices(&msg, &sigs);
            assert!(dedup_result.is_ok(), "dedup failure {dedup_result:?}");
            for passed_sigs in dedup_result.unwrap() {
                let verify_result = passed_sigs.verify_avk(&params, &ps[0].stm_signer.vk, &ps[0].stm_signer.stake, &avk, &msg);
                assert!(verify_result.is_ok(), "verify {verify_result:?}");
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that when a quorum is found, the aggregate signature can be verified by anyone with
        /// access to the avk and the parameters.
        fn test_aggregate_sig(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m, k, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let msig = clerk.aggregate(&sigs, &msg);

            match msig {
                Ok(aggr) => {
                    let verify_result = aggr.verify(&msg, &clerk.compute_avk(), &params);
                    assert!(verify_result.is_ok(), "Verification failed: {verify_result:?}");
                }
                Err(AggregationError::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k || k == params.k),
                Err(AggregationError::UsizeConversionInvalid) =>
                    unreachable!()
            }
        }

        #[test]
        /// Test that batch verification of certificates works
        fn batch_verify(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              seed in any::<[u8;32]>(),
                              batch_size in 2..10,
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut aggr_avks = Vec::new();
            let mut aggr_stms = Vec::new();
            let mut batch_msgs = Vec::new();
            let mut batch_params = Vec::new();
            for _ in 0..batch_size {
                let mut msg = [0u8; 32];
                rng.fill_bytes(&mut msg);
                let params = StmParameters { m, k, phi_f: 0.8 };
                let ps = setup_equal_parties(params, nparties);
                let clerk = StmClerk::from_signer(&ps[0]);

                let all_ps: Vec<usize> = (0..nparties).collect();
                let sigs = find_signatures(&msg, &ps, &all_ps);
                let msig = clerk.aggregate(&sigs, &msg);

                match msig {
                    Ok(aggr) => {
                        aggr_avks.push(clerk.compute_avk());
                        aggr_stms.push(aggr);
                        batch_msgs.push(msg.to_vec());
                        batch_params.push(params);
                    }
                    Err(AggregationError::NotEnoughSignatures(_n, _k)) => {
                        assert!(sigs.len() < params.k as usize)
                    }
                    Err(AggregationError::UsizeConversionInvalid) => unreachable!(),
                }
            }

            assert!(StmAggrSig::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params).is_ok());

            let mut msg = [0u8; 32];
            rng.fill_bytes(&mut msg);
            let params = StmParameters { m, k, phi_f: 0.8 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let fake_msig = clerk.aggregate(&sigs, &msg);

            aggr_stms[0] = fake_msig.unwrap();
            assert!(StmAggrSig::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params).is_err());
        }
    }

    proptest! {
        #[test]
        /// Test that when a party creates a signature it can be verified
        fn test_sig(msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 1, k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, 1);
            let clerk = StmClerk::from_signer(&ps[0]);
            let avk = clerk.compute_avk();

            if let Some(sig) = ps[0].sign(&msg) {
                assert!(sig.verify_avk(&params, &ps[0].stm_signer.vk, &ps[0].stm_signer.stake, &avk, &msg).is_ok());
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]
        #[test]
        fn test_parameters_serialize_deserialize(m in any::<u64>(), k in any::<u64>(), phi_f in any::<f64>()) {
            let params = StmParameters { m, k, phi_f };

            let bytes = params.to_bytes();
            let deserialised = StmParameters::from_bytes(&bytes);
            assert!(deserialised.is_ok())
        }

        #[test]
        fn test_initializer_serialize_deserialize(seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let params = StmParameters { m: 1, k: 1, phi_f: 1.0 };
            let stake = rng.next_u64();
            let initializer = StmInitializer::setup(params, stake, &mut rng);

            let bytes = initializer.to_bytes();
            assert!(StmInitializer::from_bytes(&bytes).is_ok());

            let bytes = bincode::serialize(&initializer).unwrap();
            assert!(bincode::deserialize::<StmInitializer>(&bytes).is_ok())
        }

        #[test]
        fn test_sig_serialize_deserialize(msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 1, k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, 1);
            let clerk = StmClerk::from_signer(&ps[0]);
            let avk = clerk.compute_avk();

            if let Some(sig) = ps[0].sign(&msg) {
                let bytes = sig.to_bytes();
                let sig_deser = StmSig::from_bytes::<D>(&bytes).unwrap();
                assert!(sig_deser.verify_avk(&params, &ps[0].stm_signer.vk, &ps[0].stm_signer.stake, &avk, &msg).is_ok());

                let encoded = bincode::serialize(&sig).unwrap();
                let decoded: StmSig = bincode::deserialize(&encoded).unwrap();
                assert!(decoded.verify_avk(&params, &ps[0].stm_signer.vk, &ps[0].stm_signer.stake, &avk, &msg).is_ok());
            }
        }

        #[test]
        fn test_multisig_serialize_deserialize(nparties in 2_usize..10,
                                          msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 10, k: 5, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let msig = clerk.aggregate(&sigs, &msg);
            if let Ok(aggr) = msig {
                    let bytes: Vec<u8> = aggr.to_bytes();
                    let aggr2 = StmAggrSig::from_bytes(&bytes).unwrap();
                    assert!(aggr2.verify(&msg, &clerk.compute_avk(), &params).is_ok());

                    let encoded = bincode::serialize(&aggr).unwrap();
                    let decoded: StmAggrSig::<D> = bincode::deserialize(&encoded).unwrap();
                    assert!(decoded.verify(&msg, &clerk.compute_avk(), &params).is_ok());
            }
        }
    }

    /// Pick N between min and max, and then
    /// generate a vector of N stakes summing to N * tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake * N
    fn arb_parties_adversary_stake(
        min: usize,
        max: usize,
        tstake: Stake,
        astake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        (min..max)
            .prop_flat_map(|n| (Just(n), 1..=n / 2))
            .prop_flat_map(move |(n, nadv)| {
                arb_parties_with_adversaries(n, nadv, tstake * n as Stake, astake * n as Stake)
            })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        /// Test that when the adversaries do not hold sufficient stake, they can not form a quorum
        fn test_adversary_quorum(
            (adversaries, parties) in arb_parties_adversary_stake(8, 30, 16, 4),
            msg in any::<[u8;16]>(),
        ) {
            // Test sanity check:
            // Check that the adversarial party has less than 40% of the total stake.
            let (good, bad) = parties.iter().enumerate().fold((0,0), |(acc1, acc2), (i, st)| {
                if adversaries.contains(&i) {
                    (acc1, acc2 + *st)
                } else {
                    (acc1 + *st, acc2)
                }
            });
            assert!(bad as f64 / ((good + bad) as f64) < 0.4);

            let params = StmParameters { m: 2642, k: 357, phi_f: 0.2 }; // From Table 1
            let ps = setup_parties(params, parties);

            let sigs =  find_signatures(&msg, &ps, &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

            let clerk = StmClerk::from_signer(&ps[0]);

            let msig = clerk.aggregate(&sigs, &msg);
            match msig {
                Err(AggregationError::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k && params.k == k),
                _ =>
                    unreachable!(),
            }
        }
    }

    #[derive(Debug)]
    struct ProofTest {
        msig: Result<Sig, AggregationError>,
        clerk: StmClerk<D>,
        msg: [u8; 16],
    }
    /// Run the protocol up to aggregation. This will produce a valid aggregation of signatures.
    /// The following tests mutate this aggregation so that the proof is no longer valid.
    fn arb_proof_setup(max_parties: usize) -> impl Strategy<Value = ProofTest> {
        any::<[u8; 16]>().prop_flat_map(move |msg| {
            (2..max_parties).prop_map(move |n| {
                let params = StmParameters {
                    m: 5,
                    k: 5,
                    phi_f: 1.0,
                };
                let ps = setup_equal_parties(params, n);
                let clerk = StmClerk::from_signer(&ps[0]);

                let all_ps: Vec<usize> = (0..n).collect();
                let sigs = find_signatures(&msg, &ps, &all_ps);

                let msig = clerk.aggregate(&sigs, &msg);
                ProofTest { msig, clerk, msg }
            })
        })
    }

    fn with_proof_mod<F>(mut tc: ProofTest, f: F)
    where
        F: Fn(&mut Sig, &mut StmClerk<D>, &mut [u8; 16]),
    {
        match tc.msig {
            Ok(mut aggr) => {
                f(&mut aggr, &mut tc.clerk, &mut tc.msg);
                assert!(aggr
                    .verify(&tc.msg, &tc.clerk.compute_avk(), &tc.clerk.params)
                    .is_err())
            }
            Err(e) => unreachable!("Reached an unexpected error: {:?}", e),
        }
    }

    proptest! {
        // Each of the tests below corresponds to falsifying a conjunct in the
        // definition of a valid signature
        #[test]
        fn test_invalid_proof_quorum(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |_aggr, clerk, _msg| {
                clerk.params.k += 7;
            })
        }
        // todo: fn test_invalid_proof_individual_sig
        #[test]
        fn test_invalid_proof_index_bound(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |_aggr, clerk, _msg| {
                clerk.params.m = 1;
            })
        }
        #[test]
        fn test_invalid_proof_index_unique(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |aggr, clerk, _msg| {
                for sig in aggr.signatures.iter_mut() {
                    for index in sig.0.indexes.iter_mut() {
                       *index %= clerk.params.k - 1
                    }
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |aggr, _, _msg| {
                let p = aggr.batch_proof.clone();
                let mut index_list = p.indices.clone();
                let values = p.values;
                let batch_proof = {
                    index_list[0] += 1;
                    BatchPath {
                        values,
                        indices: index_list,
                        hasher: Default::default()
                    }
                };
                aggr.batch_proof = batch_proof;
            })
        }
    }
}
