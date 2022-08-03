//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.
//! See figure 6 of [the paper](https://eprint.iacr.org/2021/916) for most of the
//! protocol.
//!
//! What follows is a simple example showing the usage of STM.
//!
//! ```rust
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use mithril::key_reg::KeyReg; // Import key registration functionality
//! use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSigner};
//! use mithril::error::AggregationError;
//! use rayon::prelude::*; // We use par_iter to speed things up
//!
//! use rand_chacha::ChaCha20Rng;
//! use rand_core::{RngCore, SeedableRng};
//!
//! let nparties = 4; // Use a small number of parties for this example
//! type D = blake2::Blake2b; // Setting the hash function for convenience
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
//!         .register(p.stake(), p.verification_key())
//!         .unwrap();
//!     ps.push(p);
//! }
//!
//! // Close the key registration.
//! let closed_reg = key_reg.close();
//!
//! // Finialize the StmInitializer and turn it into a StmSigner, which can execute the
//! // rest of the protocol.
//! let ps = ps
//!     .into_par_iter()
//!     .map(|p| p.new_signer(closed_reg.clone()))
//!     .collect::<Vec<StmSigner<D>>>();
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
//!     .collect::<Vec<StmSig<D>>>();
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

use crate::dense_mapping::ev_lt_phi;
use crate::error::{
    AggregationError, StmVerificationError, MultiSignatureError, RegisterError,
    StmSingleVerificationError,
};
use crate::key_reg::ClosedKeyReg;
use crate::merkle_tree::{MTLeaf, MerkleTreeCommitment, Path};
#[cfg(not(feature = "zcash"))]
use crate::multi_sig::{Signature, SigningKey, VerificationKey, VerificationKeyPoP};
#[cfg(feature = "zcash")]
use crate::multi_sig_zcash::{Signature, SigningKey, VerificationKey, VerificationKeyPoP};
use digest::{Digest, FixedOutput};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::convert::{From, TryFrom, TryInto};
use std::hash::{Hash, Hasher};

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;
/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// Used to set protocol parameters.
// todo: this is the criteria to consider parameters valid:
// Let A = max assumed adversarial stake
// Let a = A / max_stake
// Let p = φ(a)  // f needs tuning, something close to 0.2 is reasonable
// Then, we're secure if SUM[from i=k to i=m] Binomial(i successes, m experiments, p chance of success) <= 2^-100 or thereabouts.
// The latter turns to 1 - BinomialCDF(k-1,m,p)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[repr(C)]
pub struct StmParameters {
    /// Security parameter, upper bound on indices
    pub m: u64,

    /// Quorum parameter
    pub k: u64,

    /// `f` in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant.
    pub phi_f: f64,
}

impl StmParameters {
    /// Convert to bytes
    ///
    /// # Layout
    /// * Security parameter, m (as u64)
    /// * Quorum parameter, k (as u64)
    /// * Phi f, as (f64)
    pub fn to_bytes(&self) -> [u8; 24] {
        let mut out = [0; 24];
        out[..8].copy_from_slice(&self.m.to_be_bytes());
        out[8..16].copy_from_slice(&self.k.to_be_bytes());
        out[16..].copy_from_slice(&self.phi_f.to_be_bytes());
        out
    }

    /// Convert `StmParameters` from a byte slice
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

/// Wrapper of the MultiSignature Verification key with proof of possession
pub type StmVerificationKeyPoP = VerificationKeyPoP;
/// Wrapper of the MultiSignature Verification key
pub type StmVerificationKey = VerificationKey;

/// Initializer for `StmSigner`. This is the data that is used during the key registration
/// procedure. Once the latter is finished, this instance is consumed into an `StmSigner`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializer {
    /// This participant's stake
    pub(crate) stake: Stake,
    /// Current protocol instantiation parameters
    pub(crate) params: StmParameters,
    /// Secret key
    pub(crate) sk: SigningKey,
    /// Verification (public) key + proof of possession
    pub(crate) pk: StmVerificationKeyPoP,
}

/// Participant in the protocol can sign messages. This instance can only be generated out of
/// an `StmInitializer` and a `ClosedKeyReg`. This ensures that a `MerkleTree` root is not
/// computed before all participants have registered.
#[derive(Debug, Clone)]
pub struct StmSigner<D>
where
    D: Digest + FixedOutput,
{
    mt_index: u64,
    stake: Stake,
    params: StmParameters,
    sk: SigningKey,
    vk: StmVerificationKey,
    closed_reg: ClosedKeyReg<D>,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s. Clerks can only be
/// generated with the registration closed. This avoids that a Merkle Tree is computed before
/// all parties have registered.
#[derive(Debug, Clone)]
pub struct StmClerk<D>
where
    D: Clone + Digest + FixedOutput,
{
    pub(crate) closed_reg: ClosedKeyReg<D>,
    pub(crate) params: StmParameters,
}

/// Signature created by a single party who has won the lottery.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Path<D>: Serialize",
    deserialize = "Path<D>: Deserialize<'de>"
))]
pub struct StmSig<D: Clone + Digest + FixedOutput> {
    /// The signature from the underlying MSP scheme.
    pub sigma: Signature,
    /// The Stm verification Key.
    pub pk: StmVerificationKey,
    /// The stake of the party that made this signature.
    pub stake: Stake,
    /// The index(es) for which the signature is valid
    pub indexes: Vec<Index>,
    /// The path through the MerkleTree for this party.
    pub path: Path<D>,
}

impl<D: Clone + Digest + FixedOutput> Hash for StmSig<D> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.sigma.to_bytes(), state)
    }
}

// We need to implement PartialEq instead of deriving it because we are implementing Hash.
impl<D: Clone + Digest + FixedOutput> PartialEq for StmSig<D> {
    fn eq(&self, other: &Self) -> bool {
        self.sigma == other.sigma
    }
}

impl<D: Clone + Digest + FixedOutput> Eq for StmSig<D> {}

impl<D: Clone + Digest + FixedOutput> StmSig<D> {
    /// Verify an stm signature by checking that the lottery was won, the merkle path is correct,
    /// the indexes are in the desired range and the underlying msp signature validates.
    pub fn verify(
        &self,
        params: &StmParameters,
        avk: &StmAggrVerificationKey<D>,
        msg: &[u8],
    ) -> Result<(), StmSingleVerificationError<D>> {
        let msgp = avk.mt_commitment.concat_with_msg(msg);

        self.check_indices(params, &msgp, avk)?;
        if avk
            .mt_commitment
            .check(&MTLeaf(self.pk, self.stake), &self.path)
            .is_err()
        {
            Err(StmSingleVerificationError::InvalidMerkleTree(self.path.clone()))
        } else if self.sigma.verify(&msgp, &self.pk).is_err() {
            Err(StmSingleVerificationError::InvalidSignature(self.sigma))
        } else {
            Ok(())
        }
    }

    pub(crate) fn check_indices(
        &self,
        params: &StmParameters,
        msgp: &[u8],
        avk: &StmAggrVerificationKey<D>,
    ) -> Result<(), StmSingleVerificationError<D>> {
        for &index in &self.indexes {
            if index > params.m {
                return Err(StmSingleVerificationError::IndexBoundFailed(index, params.m));
            }

            let ev = self.sigma.eval(msgp, index);

            if !ev_lt_phi(params.phi_f, ev, self.stake, avk.total_stake) {
                return Err(StmSingleVerificationError::LotteryLost);
            }
        }

        Ok(())
    }

    /// Convert an `StmSig` into bytes
    ///
    /// # Layout
    ///
    /// * Party id
    /// * Stake
    /// * Number of valid indexes
    /// * Indexes of the signature
    /// * Public Key
    /// * Msp Signature
    /// * Merkle Path for (Public Key, Stake)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&self.stake.to_be_bytes());
        output.extend_from_slice(&(self.indexes.len() as u64).to_be_bytes());
        for index in &self.indexes {
            output.extend_from_slice(&index.to_be_bytes());
        }
        output.extend_from_slice(&self.pk.to_bytes());
        output.extend_from_slice(&self.sigma.to_bytes());
        output.extend_from_slice(&self.path.to_bytes());
        output
    }

    /// Convert an `StmSig` from a byte slice
    pub fn from_bytes(bytes: &[u8]) -> Result<StmSig<D>, MultiSignatureError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let stake = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let nr_indexes = u64::from_be_bytes(u64_bytes) as usize;
        let mut index = Vec::new();
        for i in 0..nr_indexes {
            u64_bytes.copy_from_slice(&bytes[16 + i * 8..24 + i * 8]);
            index.push(u64::from_be_bytes(u64_bytes));
        }
        let pk = StmVerificationKey::from_bytes(&bytes[16 + nr_indexes * 8..])?;
        let sigma = Signature::from_bytes(&bytes[112 + nr_indexes * 8..])?;
        let path = Path::from_bytes(&bytes[160 + nr_indexes * 8..])?;

        Ok(StmSig {
            sigma,
            pk,
            stake,
            indexes: index,
            path,
        })
    }
}

/// Stm aggregate key, which contains the merkle tree root, and the total stake of the system.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Path<D>: Serialize",
    deserialize = "Path<D>: Deserialize<'de>"
))]
pub struct StmAggrVerificationKey<D>
where
    D: Clone + Digest + FixedOutput,
{
    mt_commitment: MerkleTreeCommitment<D>,
    total_stake: Stake,
}

impl<D> From<&ClosedKeyReg<D>> for StmAggrVerificationKey<D>
where
    D: Clone + Digest + FixedOutput,
{
    fn from(reg: &ClosedKeyReg<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_commitment(),
            total_stake: reg.total_stake,
        }
    }
}

/// `StmMultiSig` uses the "concatenation" proving system. This means that the aggregated
/// signature contains a vector of the individual signatures.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "Path<D>: Serialize",
    deserialize = "Path<D>: Deserialize<'de>"
))]
pub struct StmAggrSig<D>
where
    D: Clone + Digest + FixedOutput,
{
    pub(crate) signatures: Vec<StmSig<D>>,
}

impl<D: Clone + Digest + FixedOutput> StmAggrSig<D> {
    /// Verify aggregate signature
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &StmAggrVerificationKey<D>,
        parameters: &StmParameters,
    ) -> Result<(), StmVerificationError<D>> {
        // Check that indices are all smaller than `m` and they are unique
        let mut nr_indices = 0;
        let mut unique_indices = HashSet::new();
        for sig in &self.signatures {
            for &index in &sig.indexes {
                if index > parameters.m {
                    println!("one");
                    return Err(StmVerificationError::IndexBoundFailed(index, parameters.m));
                }
                unique_indices.insert(index);
                nr_indices += 1;
            }
        }

        if nr_indices != unique_indices.len() {
            println!("two");
            return Err(StmVerificationError::IndexNotUnique);
        }

        // Check that there are sufficient signatures
        if (nr_indices as u64) < parameters.k {
            println!("three");
            return Err(StmVerificationError::NoQuorum);
        }

        // Check that all signatures did win the lottery
        for sig in self.signatures.iter() {
            let msgp = avk.mt_commitment.concat_with_msg(msg);
            sig.check_indices(parameters, &msgp, avk)?;

            // Check that merkle paths are valid
            if avk
                .mt_commitment
                .check(&MTLeaf(sig.pk, sig.stake), &sig.path)
                .is_err()
            {
                println!("four");
                return Err(StmVerificationError::PathInvalid(sig.path.clone()));
            }
        }

        let msg = avk.mt_commitment.concat_with_msg(msg);
        let signatures = self
            .signatures
            .iter()
            .map(|sig| sig.sigma)
            .collect::<Vec<Signature>>();
        let vks = self
            .signatures
            .iter()
            .map(|sig| sig.pk)
            .collect::<Vec<VerificationKey>>();

        Signature::verify_aggregate(msg.as_slice(), &vks, &signatures)?;
        Ok(())
    }

    /// Convert multi signature to bytes
    ///
    /// # Layout
    /// * Number of signatures
    /// * Size of a signature
    /// * Signatures
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&u64::try_from(self.signatures.len()).unwrap().to_be_bytes());
        out.extend_from_slice(
            &u64::try_from(self.signatures[0].to_bytes().len())
                .unwrap()
                .to_be_bytes(),
        );
        for sig in &self.signatures {
            out.extend_from_slice(&sig.to_bytes())
        }
        out
    }

    /// Convert a `StmMultiSig` from a byte slice
    pub fn from_bytes(bytes: &[u8]) -> Result<StmAggrSig<D>, MultiSignatureError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MultiSignatureError::SerializationError)?;
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let sig_size = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MultiSignatureError::SerializationError)?;
        let mut signatures = Vec::with_capacity(size);
        for i in 0..size {
            signatures.push(StmSig::from_bytes(
                &bytes[16 + i * sig_size..16 + (i + 1) * sig_size],
            )?);
        }

        Ok(StmAggrSig { signatures })
    }
}

impl StmInitializer {
    /// Builds an `StmInitializer` that is ready to register with the key registration service
    // todo: definitely don't like how the id is handled. To initialise one needs to be aware of the id?
    pub fn setup<R>(params: StmParameters, stake: Stake, rng: &mut R) -> Self
    where
        R: RngCore + CryptoRng,
    {
        let sk = SigningKey::gen(rng);
        let pk = StmVerificationKeyPoP::from(&sk);
        Self {
            stake,
            params,
            sk,
            pk,
        }
    }

    /// Create a new key.
    pub fn generate_new_key<R>(&mut self, rng: &mut R)
    where
        R: RngCore + CryptoRng,
    {
        let sk = SigningKey::gen(rng);
        let pk = StmVerificationKeyPoP::from(&sk);
        self.sk = sk;
        self.pk = pk;
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKeyPoP {
        self.pk
    }

    /// Set a new pair of keys out of a secret key
    // todo: mmmh, do we need this?
    pub fn set_key(&mut self, sk: SigningKey) {
        let pk = StmVerificationKeyPoP::from(&sk);
        self.sk = sk;
        self.pk = pk;
    }

    /// Set the stake.
    pub fn set_stake(&mut self, stake: Stake) {
        self.stake = stake;
    }

    /// Get the stake.
    pub fn stake(&self) -> Stake {
        self.stake
    }

    /// Set the StmParameters.
    pub fn set_params(&mut self, params: StmParameters) {
        self.params = params;
    }

    /// Get the parameters
    pub fn params(&self) -> StmParameters {
        self.params
    }

    /// Build the avk for the given list of parties.
    ///
    /// Note that if this StmInitializer was modified *between* the last call to `register`,
    /// then the resulting `StmSigner` may not be able to produce valid signatures.
    ///
    /// Returns an StmSigner specialized to
    /// (1) this StmSigner's ID and current stake
    /// (2) this StmSigner's parameter valuation
    /// (3) the avk as built from the current registered parties (according to the registration service)
    /// (4) the current total stake (according to the registration service)
    pub fn new_signer<D>(self, closed_reg: ClosedKeyReg<D>) -> StmSigner<D>
    where
        D: Digest + FixedOutput + Clone,
    {
        // Extract this signer's party index from the registry, i.e. the position of the merkle
        // tree leaf.
        let mut my_index = None;
        for (i, rp) in closed_reg.reg_parties.iter().enumerate() {
            if rp.0 == self.pk.vk {
                my_index = Some(i as u64);
                break;
            }
        }
        StmSigner {
            mt_index: my_index.unwrap_or_else(|| {
                panic!("Initializer not registered. Cannot participate as a signer.",)
            }),
            stake: self.stake,
            params: self.params,
            sk: self.sk,
            vk: self.pk.vk,
            closed_reg,
        }
    }

    /// Convert to bytes
    ///
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

impl<D> StmSigner<D>
where
    D: Clone + Digest + FixedOutput,
{
    /// If lottery is won for this message/index, signs it.
    pub fn sign(&self, msg: &[u8]) -> Option<StmSig<D>> {
        let msgp = self
            .closed_reg
            .merkle_tree
            .to_commitment()
            .concat_with_msg(msg);
        let sigma = self.sk.sign(&msgp);

        // Check which lotteries are won
        let mut indexes = Vec::new();
        for index in 0..self.params.m {
            if ev_lt_phi(
                self.params.phi_f,
                sigma.eval(&msgp, index),
                self.stake,
                self.closed_reg.total_stake,
            ) {
                indexes.push(index);
            }
        }
        if !indexes.is_empty() {
            let path = self
                .closed_reg
                .merkle_tree
                .get_path(self.mt_index.try_into().ok()?);
            Some(StmSig {
                sigma,
                pk: self.vk,
                stake: self.stake,
                indexes,
                path,
            })
        } else {
            None
        }
    }

    /// This function should be called when a signing epoch is finished (or when a new one starts).
    /// It consumes `self` and turns it back to an `StmInitializer`, which allows for an update in
    /// the dynamic parameters (such as stake distribution, or participants). To ensure that the
    /// `StmInitializer` will not be used for the previous registration, this function also consumes
    /// the `ClosedKeyReg` instance. In case the stake of the current party has changed, it includes
    /// it as input.
    ///
    /// # Example
    /// ```
    /// # use mithril::key_reg::{ClosedKeyReg, KeyReg};
    /// # use mithril::stm::{Stake, StmInitializer, StmParameters, StmVerificationKeyPoP};
    /// # use rand_chacha::ChaCha20Rng;
    /// # use rand_core::{RngCore, SeedableRng};
    /// # use blake2::{Blake2b, Digest};
    ///
    /// # fn main() {
    ///     // Parameter. This information is broadcast (or known) to all
    ///     // participants.
    /// let params = StmParameters {
    ///         k: 3,
    ///         m: 10,
    ///         phi_f: 1.0,
    ///     };
    ///
    ///     let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    ///
    ///     ////////////////////////////////
    ///     //////// EPOCH 1 ///////////////
    ///     ////////////////////////////////
    ///
    ///     // The example starts with only two parties.
    ///     let nparties_e1 = 2;
    ///
    ///     // We initialise the stake at epoch 1
    ///     let mut total_stake_e1: Stake = 0;
    ///     let stakes_e1 = (0..nparties_e1)
    ///         .into_iter()
    ///         .map(|_| {
    ///             let stake = rng.next_u64() % 999;
    ///             total_stake_e1 += stake;
    ///             1 + stake
    ///         })
    ///         .collect::<Vec<_>>();
    ///
    ///     // Each party generates their Stm keys
    ///     let party_0_init_e1 = StmInitializer::setup(params, stakes_e1[0], &mut rng);
    ///     let party_1_init_e1 = StmInitializer::setup(params, stakes_e1[1], &mut rng);
    ///
    ///     // The public keys are broadcast. All participants will have the same keys. We expect
    ///     // the keys to be persistent.
    ///     let mut parties_pks: Vec<StmVerificationKeyPoP> = vec![
    ///         party_0_init_e1.verification_key(),
    ///         party_1_init_e1.verification_key(),
    ///     ];
    ///
    ///     // Now, each party registers all other participating parties. Once all parties are registered, the key registration
    ///     // is closed.
    ///     let party_0_key_reg_e1 = local_reg(&stakes_e1, &parties_pks);
    ///     let party_1_key_reg_e1 = local_reg(&stakes_e1, &parties_pks);
    ///
    ///     // Now, with information of all participating parties, the
    ///     // signers can be initialised (we can create the Merkle Tree).
    ///     // The (closed) key registration is consumed, to ensure that it
    ///     // is not used to initialise a signer at a different epoch.
    ///     let party_0 = party_0_init_e1.new_signer(party_0_key_reg_e1);
    ///     let party_1 = party_1_init_e1.new_signer(party_1_key_reg_e1);
    ///
    ///     ////////////////////////////////
    ///     //////// EPOCH 2 ///////////////
    ///     ////////////////////////////////
    ///
    ///     // Now the second epoch starts. A new party joins, and the stake of all
    ///     // signers changes.
    ///     let nparties_e2 = 3;
    ///
    ///     // We initialise the stake at epoch 2
    ///     let mut total_stake_e2: Stake = 0;
    ///     let stakes_e2 = (0..nparties_e2)
    ///         .into_iter()
    ///         .map(|_| {
    ///             let stake = rng.next_u64() % 999;
    ///             total_stake_e2 += stake;
    ///             1 + stake
    ///         })
    ///         .collect::<Vec<_>>();
    ///
    ///     // Now the `StmSigner`s are outdated with respect to the new stake, and participants.
    ///     // We allow a transition from `StmSigner` back to `StmInitializer`:
    ///     let party_0_init_e2 = party_0.new_epoch(Some(stakes_e2[0]));
    ///     let party_1_init_e2 = party_1.new_epoch(Some(stakes_e2[1]));
    ///
    ///     // The third party needs to generate from scratch and broadcast the key (which we represent
    ///     // by appending to the `pks` vector.
    ///     let party_2_init_e2 = StmInitializer::setup(params, stakes_e2[2], &mut rng);
    ///     parties_pks.push(party_2_init_e2.verification_key());
    ///
    ///     // The key reg of epoch 1 was consumed, so it cannot be used to generate a signer.
    ///     // This forces us to re-run the key registration (which is good).
    ///     let key_reg_e2_0 = local_reg(&stakes_e2, &parties_pks);
    ///     let key_reg_e2_1 = local_reg(&stakes_e2, &parties_pks);
    ///     let key_reg_e2_2 = local_reg(&stakes_e2, &parties_pks);
    ///
    ///     // And finally, new signers can be created to signe messages in epoch 2. Again, signers
    ///     // of epoch 1 are consumed, so they cannot be used to sign messages of this epoch (again,
    ///     // this is good).
    ///     let _party_0_e2 = party_0_init_e2.new_signer(key_reg_e2_0);
    ///     let _party_1_e2 = party_1_init_e2.new_signer(key_reg_e2_1);
    ///     let _party_2_e2 = party_2_init_e2.new_signer(key_reg_e2_2);
    /// # }
    ///
    /// # fn local_reg(stakes: &[u64], pks: &[StmVerificationKeyPoP]) -> ClosedKeyReg<Blake2b> {
    /// # let mut local_keyreg = KeyReg::init();
    /// #    for (&pk, stake) in pks.iter().zip(stakes.iter()) {
    /// #        local_keyreg.register(*stake, pk).unwrap();
    /// #    }
    /// #    local_keyreg.close()
    /// # }
    /// ```
    pub fn new_epoch(self, new_stake: Option<Stake>) -> StmInitializer {
        let stake = match new_stake {
            None => self.stake,
            Some(s) => s,
        };

        StmInitializer {
            stake,
            params: self.params,
            pk: StmVerificationKeyPoP::from(&self.sk),
            sk: self.sk,
        }
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        StmAggrVerificationKey::from(&self.closed_reg)
    }
}

impl<D> StmClerk<D>
where
    D: Digest + FixedOutput + Clone,
{
    /// Create a new `Clerk` from a closed registration instance.
    /// todo: why does it consume the closed reg?
    pub fn from_registration(params: StmParameters, closed_reg: ClosedKeyReg<D>) -> Self {
        Self { params, closed_reg }
    }

    /// Creates a Clerk from a Signer.
    pub fn from_signer(signer: &StmSigner<D>) -> Self {
        Self {
            params: signer.params,
            closed_reg: signer.closed_reg.clone(),
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// The `From` bound on `Proof::Statement` allows `aggregate` to translate
    /// from the `Mithril` specific statement and witness types to their proof system-specific
    /// representations.
    pub fn aggregate(
        &self,
        sigs: &[StmSig<D>],
        msg: &[u8],
    ) -> Result<StmAggrSig<D>, AggregationError> {
        // todo: how come the dedup does not take the concatenated message
        // let msgp = concat_avk_with_msg(&self.avk.to_commitment(), msg);
        let unique_sigs = self.dedup_sigs_for_indices(msg, sigs)?;

        Ok(StmAggrSig {
            signatures: unique_sigs,
        })
    }

    /// Given a slice of `sigs`, this functions returns a new list of signatures with only
    /// valid indices.
    /// In case of conflict (having several signatures for the same index) it selects the
    /// smallest signature (i.e. takes the signature with the smallest scalar). The function
    /// selects at least `self.k` indexes. If there is no sufficient, then returns an error.
    /// todo: We need to agree on a criteria to dedup
    pub fn dedup_sigs_for_indices(
        &self,
        msg: &[u8],
        sigs: &[StmSig<D>],
    ) -> Result<Vec<StmSig<D>>, AggregationError> {
        let avk = StmAggrVerificationKey::from(&self.closed_reg);
        let mut sig_by_index: HashMap<Index, &StmSig<D>> = HashMap::new();
        let mut removal_idx_by_vk: HashMap<&StmSig<D>, Vec<Index>> = HashMap::new();

        for sig in sigs.iter() {
            if sig.verify(&self.params, &avk, msg).is_err() {
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

        let mut dedup_sigs: HashSet<StmSig<D>> = HashSet::new();
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

        Err(AggregationError::NotEnoughSignatures(
            count,
            self.params.k,
        ))
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        StmAggrVerificationKey::from(&self.closed_reg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key_reg::*;
    use bincode;
    use blake2::Blake2b;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use proptest::test_runner::{RngAlgorithm::ChaCha, TestRng};
    use std::collections::{HashMap, HashSet};

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type Sig = StmAggrSig<D>;
    type D = Blake2b;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSigner<D>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSigner<D>> {
        let mut kr = KeyReg::init();
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = ChaCha20Rng::from_seed(trng.gen());
        // The needless_collect lint is not correct here
        #[allow(clippy::needless_collect)]
        let ps = stake
            .into_iter()
            .map(|stake| {
                let p = StmInitializer::setup(params, stake, &mut rng);
                kr.register(stake, p.verification_key()).unwrap();
                p
            })
            .collect::<Vec<_>>();
        let closed_reg = kr.close();
        ps.into_iter()
            .map(|p| p.new_signer(closed_reg.clone()))
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

                let adversaries = adversaries.into_iter().map(|(key, _)| key).collect();
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

    fn find_signatures(msg: &[u8], ps: &[StmSigner<D>], is: &[usize]) -> Vec<StmSig<D>> {
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
            let nparties = 10usize;
            let false_msg = [1u8; 20];
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p);
            let avk = clerk.compute_avk();
            let mut sigs = Vec::with_capacity(nparties);


            if let Some(sig) = p.sign(&false_msg) {
                sigs.push(sig);
            }

            if let Some(sig) = p.sign(&msg) {
                sigs.push(sig);
            }

            let dedup_result = clerk.dedup_sigs_for_indices(&msg, &sigs);
            assert!(dedup_result.is_ok(), "dedup failure {:?}", dedup_result);
            for passed_sigs in dedup_result.unwrap() {
                let verify_result = passed_sigs.verify(&params, &avk, &msg);
                assert!(verify_result.is_ok(), "verify {:?}", verify_result);
            }
        }
    }

    proptest! {
        #[test]
        /// Test that when a party creates a signature it can be verified
        fn test_sig(msg in any::<[u8;16]>()) {
            let nparties = 2;
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p);
            let avk = clerk.compute_avk();

            if let Some(sig) = p.sign(&msg) {
                assert!(sig.verify(&params, &avk, &msg).is_ok());
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
                    assert!(verify_result.is_ok(), "{:?}", verify_result);
                }
                Err(AggregationError::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k || k == params.k),
                Err(AggregationError::InvalidUsizeConversion) =>
                    unreachable!()
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
            let nparties = 2;
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p);
            let avk = clerk.compute_avk();

            if let Some(sig) = p.sign(&msg) {
                let bytes = sig.to_bytes();
                let sig_deser = StmSig::<D>::from_bytes(&bytes).unwrap();
                assert!(sig_deser.verify(&params, &avk, &msg).is_ok());

                let encoded = bincode::serialize(&sig).unwrap();
                let decoded: StmSig::<D> = bincode::deserialize(&encoded).unwrap();
                assert!(decoded.verify(&params, &avk, &msg).is_ok());
            }
        }

        #[test]
        fn test_multisig_serialize_deserialize(nparties in 2_usize..10,
                                          msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 10, k: 1, phi_f: 1.0 };
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
        n: usize,
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
                ProofTest {
                    n,
                    msig,
                    clerk,
                    msg,
                }
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
                    for index in sig.indexes.iter_mut() {
                       *index %= clerk.params.k - 1
                    }
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10), i in any::<usize>()) {
            let n = tc.n;
            with_proof_mod(tc, |aggr, _, _msg| {
                let pi = i % aggr.signatures.len();
                aggr.signatures[pi].path.index = (aggr.signatures[pi].path.index + 1) % n;
            })
        }
    }
}
