//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.
//! See figure 6 of [the paper](https://eprint.iacr.org/2021/916) for most of the
//! protocol.
//!
//! What follows is a simple example showing the usage of STM.
//!
//! ```rust
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use mithril::key_reg::KeyReg; // Import key registration functionality
//! use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSigner, MTValue};
//! use mithril::error::AggregationFailure;
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
//!     k: 2, // Qorum parameter XXX: not for production
//!     phi_f: 0.2, // Lottery parameter XXX: not for production
//! };
//!
//! // Generate some arbitrary stake for each party
//! // Stake is an integer.
//! // Total stake of all parties is total stake in the system.
//! let parties = (0..nparties)
//!     .into_iter()
//!     .map(|pid| (pid as u64, 1 + (rng.next_u64() % 9999)))
//!     .collect::<Vec<_>>();
//!
//! // Create a new key registry from the parties and their stake
//! let mut key_reg = KeyReg::new(&parties);
//!
//! // For each party, crate a StmInitializer.
//! // This struct can create keys for the party.
//! let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties);
//! for (pid, stake) in parties {
//!     // Create keys for this party
//!     let p = StmInitializer::setup(params, pid, stake, &mut rng);
//!     // Register keys with the KeyReg service
//!     key_reg
//!         .register(p.party_id(), p.verification_key())
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
//! let p_results = ps
//!     .par_iter()
//!     .map(|p| {
//!         // Now for party p we try to sign the msg with each index.
//!         let mut sigs = Vec::new();
//!         let mut ixs = Vec::new();
//!         for ix in 1..params.m {
//!             // If party p wins the lottery for index ix, then sign the message
//!             if let Some(sig) = p.sign(&msg, ix) {
//!                 sigs.push(sig);
//!                 ixs.push(ix);
//!             }
//!         }
//!         (ixs, sigs)
//!     })
//!     .collect::<Vec<_>>();
//! let mut sigs = Vec::new();
//! let mut ixs = Vec::new();
//! for res in p_results {
//!     ixs.extend(res.0);
//!     sigs.extend(res.1);
//! }
//!
//! // StmClerk can aggregate and verify signatures.
//! let clerk = StmClerk::from_signer(&ps[0]);
//!
//! // Aggregate and verify the signatures
//! let msig = clerk.aggregate(&sigs,&msg);
//! match msig {
//!     Ok(aggr) => {
//!         println!("Aggregate ok");
//!         assert!(clerk
//!             .verify_msig(&aggr, &msg)
//!             .is_ok());
//!     }
//!     Err(AggregationFailure::NotEnoughSignatures(n, k)) => {
//!         println!("Not enough signatures");
//!         assert!(n < params.k && k == params.k)
//!     }
//!     Err(_) => unreachable!(),
//! }
//! # Ok(())
//! # }
//! ```

use crate::error::{
    AggregationFailure, MithrilWitnessError, MultiSignatureError, RegisterError,
    VerificationFailure,
};
use crate::key_reg::ClosedKeyReg;
use crate::merkle_tree::{concat_avk_with_msg, MerkleTree, MerkleTreeCommitment, Path};
use crate::msp::{VerificationKey, VerificationKeyPoP, Signature, SigningKey};
use digest::{Digest, FixedOutput};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::convert::{From, TryFrom, TryInto};
use std::sync::Arc;
#[cfg(feature = "num-integer-backend")]
use {
    num_bigint::{BigInt, Sign},
    num_rational::Ratio,
    num_traits::{One, Signed},
    std::ops::Neg,
};

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;
/// Party identifier, unique for each participant in the protocol.
pub type PartyId = u64;
/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// The values that are represented in the Merkle Tree.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct MTValue(pub VerificationKey, pub Stake);

impl MTValue {
    pub(crate) fn to_bytes(self) -> [u8; 104] {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result
    }
}

/// Used to set protocol parameters.
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

/// Wrapper of the MultiSignature Verification key
pub type StmVerificationKey = VerificationKeyPoP;

/// Initializer for `StmSigner`. This is the data that is used during the key registration
/// procedure. One the latter is finished, this instance is consumed into an `StmSigner`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmInitializer {
    /// This participant's Id
    pub(crate) party_id: PartyId,
    /// This participant's stake
    pub(crate) stake: Stake,
    /// Current protocol instantiation parameters
    pub(crate) params: StmParameters,
    /// Secret key
    pub(crate) sk: SigningKey,
    /// Verification (public) key + proof of possession
    pub(crate) pk: StmVerificationKey,
}

/// Participant in the protocol. Can sign messages. This instance can only be generated out of
/// an `StmInitializer` and a closed `KeyReg`. This ensures that a `MerkleTree` root is not
/// computed before all participants have registered.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmSigner<D>
where
    D: Digest + FixedOutput,
{
    party_id: PartyId,
    mt_index: u64,
    stake: Stake,
    params: StmParameters,
    avk: Arc<MerkleTree<D>>,
    sk: SigningKey,
    pk: StmVerificationKey,
    total_stake: Stake,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s. Clerks can only be
/// generated with the registration closed. This avoids that a Merkle Tree is computed before
/// all parties have registered.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmClerk<D>
where
    D: Clone + Digest + FixedOutput,
{
    avk: Arc<MerkleTree<D>>,
    params: StmParameters,
    total_stake: Stake,
}

/// Signature created by a single party who has won the lottery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmSig<D: Clone + Digest + FixedOutput> {
    /// The signature from the underlying MSP scheme.
    pub sigma: Signature,
    /// The pubkey from the underlying MSP scheme.
    // todo: do we need the full pk? or just the mvk?
    pub pk: StmVerificationKey,
    /// The party that made this signature.
    pub party: PartyId,
    /// The stake of the party that made this signature.
    pub stake: Stake,
    /// The index for which the signature is valid
    pub index: Index,
    /// The path through the MerkleTree for this party.
    pub path: Path<D>,
}

impl<D: Clone + Digest + FixedOutput> StmSig<D> {
    /// Convert an `StmSig` into bytes
    ///
    /// # Layout
    ///
    /// * Party id
    /// * Stake
    /// * Index of the signature
    /// * Public Key
    /// * Msp Signature
    /// * Merkle Path for (Public Key, Stake)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&self.party.to_be_bytes());
        output.extend_from_slice(&self.stake.to_be_bytes());
        output.extend_from_slice(&self.index.to_be_bytes());
        output.extend_from_slice(&self.pk.to_bytes());
        output.extend_from_slice(&self.sigma.to_bytes());
        output.extend_from_slice(&self.path.to_bytes());
        output
    }

    /// Convert an `StmSig` from a byte slice
    pub fn from_bytes(bytes: &[u8]) -> Result<StmSig<D>, MultiSignatureError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let party = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let stake = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[16..24]);
        let index = u64::from_be_bytes(u64_bytes);
        let pk = StmVerificationKey::from_bytes(&bytes[24..])?;
        let sigma = Signature::from_bytes(&bytes[216..])?;
        let path = Path::from_bytes(&bytes[264..])?;

        Ok(StmSig {
            sigma,
            pk,
            party,
            stake,
            index,
            path,
        })
    }
}

/// `StmMultiSig` uses the "concatenation" proving system. This means that the aggregated
/// signature contains a vector of the individual signatures.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmMultiSig<D>
where
    D: Clone + Digest + FixedOutput,
{
    pub(crate) signatures: Vec<StmSig<D>>,
}

impl<D: Clone + Digest + FixedOutput> StmMultiSig<D> {
    /// Verify aggregate signature
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &MerkleTreeCommitment<D>,
        parameters: &StmParameters,
        total_stake: Stake,
    ) -> Result<(), MithrilWitnessError<D>> {
        // Check that indices are all smaller than `m` and they are unique
        if self
            .signatures
            .iter()
            .map(|sig| {
                if sig.index > parameters.m {
                    return Err(MithrilWitnessError::IndexBoundFailed(
                        sig.index,
                        parameters.m,
                    ));
                }
                Ok(sig.index)
            })
            .collect::<Result<HashSet<Index>, MithrilWitnessError<D>>>()?
            .len()
            != self.signatures.len()
        {
            return Err(MithrilWitnessError::IndexNotUnique);
        }

        // Check that there are sufficient signatures
        if (self.signatures.len() as u64) < parameters.k {
            return Err(MithrilWitnessError::NoQuorum);
        }

        // Check that all signatures did win the lottery
        for sig in self.signatures.iter() {
            let msgp = concat_avk_with_msg(avk, msg);
            let ev = sig.sigma.eval(&msgp, sig.index);

            if !ev_lt_phi(parameters.phi_f, ev, sig.stake, total_stake) {
                return Err(MithrilWitnessError::EvalInvalid(ev));
            }

            // Check that merkle paths are valid
            if avk
                .check(&MTValue(sig.pk.vk, sig.stake).to_bytes(), &sig.path)
                .is_err()
            {
                return Err(MithrilWitnessError::PathInvalid(sig.path.clone()));
            }

            // Check that signatures are valid
            // todo: wait, WHAT?? if ok it fails?
            if sig.sigma.verify(msg, &sig.pk.vk).is_ok() {
                return Err(MithrilWitnessError::InvalidSignature(sig.sigma));
            }
        }

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
    pub fn from_bytes(bytes: &[u8]) -> Result<StmMultiSig<D>, MultiSignatureError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let size = usize::try_from(u64::from_be_bytes(u64_bytes)).unwrap();
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let sig_size = usize::try_from(u64::from_be_bytes(u64_bytes)).unwrap();
        let mut signatures = Vec::with_capacity(size);
        for i in 0..size {
            signatures.push(StmSig::from_bytes(
                &bytes[16 + i * sig_size..16 + (i + 1) * sig_size],
            )?);
        }

        Ok(StmMultiSig { signatures })
    }
}

impl StmInitializer {
    /// Builds an `StmInitializer` that is ready to register with the key registration service
    // todo: definitely don't like how the id is handled. To initialise one needs to be aware of the id?
    pub fn setup<R>(params: StmParameters, party_id: PartyId, stake: Stake, rng: &mut R) -> Self
    where
        R: RngCore + CryptoRng,
    {
        let sk = SigningKey::gen(rng);
        let pk = StmVerificationKey::from(&sk);
        Self {
            party_id,
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
        let pk = StmVerificationKey::from(&sk);
        self.sk = sk;
        self.pk = pk;
    }

    /// Extract the secret key.
    pub fn secret_key(&self) -> SigningKey {
        self.sk.clone()
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> StmVerificationKey {
        self.pk
    }

    /// Set a new pair of keys out of a secret key
    // todo: mmmh, do we need this?
    pub fn set_key(&mut self, sk: SigningKey) {
        let pk = StmVerificationKey::from(&sk);
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

    /// Get the party ID.
    pub fn party_id(&self) -> PartyId {
        self.party_id
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
        for (i, rp) in closed_reg.retrieve_all().iter().enumerate() {
            if rp.party_id == self.party_id {
                my_index = Some(i as u64);
                break;
            }
        }
        StmSigner {
            party_id: self.party_id,
            mt_index: my_index.unwrap_or_else(|| {
                panic!(
                    "Initializer not registered: {}. Cannot participate as a signer.",
                    self.party_id
                )
            }),
            stake: self.stake,
            params: self.params,
            avk: closed_reg.avk,
            sk: self.sk,
            pk: self.pk,
            total_stake: closed_reg.total_stake,
        }
    }

    /// Convert to bytes
    ///
    /// # Layout
    /// * Party identifier (u64)
    /// * Stake (u64)
    /// * Params
    /// * Secret Key
    /// * Public key (including PoP)
    pub fn to_bytes(&self) -> [u8; 264] {
        let mut out = [0u8; 264];
        out[..8].copy_from_slice(&self.party_id.to_be_bytes());
        out[8..16].copy_from_slice(&self.stake.to_be_bytes());
        out[16..40].copy_from_slice(&self.params.to_bytes());
        out[40..72].copy_from_slice(&self.sk.to_bytes());
        out[72..].copy_from_slice(&self.pk.to_bytes());
        out
    }

    /// Convert a slice of bytes to an `StmInitializer`
    pub fn from_bytes(bytes: &[u8]) -> Result<StmInitializer, RegisterError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let party_id = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let stake = u64::from_be_bytes(u64_bytes);
        let params = StmParameters::from_bytes(&bytes[16..])?;
        let sk = SigningKey::from_bytes(&bytes[40..])?;
        let pk = StmVerificationKey::from_bytes(&bytes[72..])?;

        Ok(Self {
            party_id,
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
    /////////////////////
    // Operation phase //
    /////////////////////
    /// Try to win the lottery for this message/index combo.
    pub fn eligibility_check(&self, msg: &[u8], index: Index) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let msgp = concat_avk_with_msg(&self.avk.to_commitment(), msg);
        let sigma = self.sk.sign(&msgp);
        let ev = sigma.eval(&msgp, index);
        ev_lt_phi(self.params.phi_f, ev, self.stake, self.total_stake)
    }

    /// If lottery is won for this message/index, signs it.
    pub fn sign(&self, msg: &[u8], index: Index) -> Option<StmSig<D>> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = concat_avk_with_msg(&self.avk.to_commitment(), msg);
            let sigma = self.sk.sign(&msgp);
            let path = self.avk.get_path(self.mt_index.try_into().unwrap());
            Some(StmSig {
                sigma,
                pk: self.pk,
                party: self.mt_index,
                stake: self.stake,
                index,
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
    /// See an example [here](mithril::examples::dynamic_stake).
    pub fn new_epoch(self, new_stake: Option<Stake>) -> StmInitializer {
        let stake = match new_stake {
            None => self.stake,
            Some(s) => s,
        };

        StmInitializer {
            party_id: self.party_id,
            stake,
            params: self.params,
            sk: self.sk,
            pk: self.pk,
        }
    }

    /// Return a byte string
    ///
    /// # Layout
    /// * party_id,
    /// * mt_index,
    /// * stake,
    /// * total_stake,
    /// * params,
    /// * sk,
    /// * pk,
    /// * avk,
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&self.party_id.to_be_bytes());
        output.extend_from_slice(&self.mt_index.to_be_bytes());
        output.extend_from_slice(&self.stake.to_be_bytes());
        output.extend_from_slice(&self.total_stake.to_be_bytes());
        output.extend_from_slice(&self.params.to_bytes());
        output.extend_from_slice(&self.sk.to_bytes());
        output.extend_from_slice(&self.pk.to_bytes());
        output.extend_from_slice(&self.avk.to_bytes());

        output
    }

    /// Convert a byte string into an `StmSigner`
    ///
    /// # Panics
    /// todo: we probably want this in all `from_bytes` fns
    /// If the slice has not the expected size, the function fails.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let party_id = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let mt_index = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[16..24]);
        let stake = u64::from_be_bytes(u64_bytes);
        u64_bytes.copy_from_slice(&bytes[24..32]);
        let total_stake = u64::from_be_bytes(u64_bytes);
        let params = StmParameters::from_bytes(&bytes[32..])?;
        let sk = SigningKey::from_bytes(&bytes[56..])?;
        let pk = StmVerificationKey::from_bytes(&bytes[88..])?;

        let avk = Arc::new(MerkleTree::from_bytes(&bytes[280..])?);

        Ok(Self {
            party_id,
            mt_index,
            stake,
            params,
            avk,
            sk,
            pk,
            total_stake,
        })
    }
}

impl<D> StmClerk<D>
where
    D: Digest + FixedOutput + Clone,
{
    /// Create a new `Clerk` from a closed registration instance.
    /// todo: why does it consume the closed reg?
    pub fn from_registration(params: StmParameters, closed_reg: ClosedKeyReg<D>) -> Self {
        Self {
            params,
            avk: closed_reg.avk,
            total_stake: closed_reg.total_stake,
        }
    }

    /// Creates a Clerk from a Signer.
    pub fn from_signer(signer: &StmSigner<D>) -> Self {
        Self {
            params: signer.params,
            avk: signer.avk.clone(),
            total_stake: signer.total_stake,
        }
    }

    /// Verify an stm signature by checking that the lottery was won, the merkle path is correct and the
    /// underlying msp signature validates.
    /// todo: I may prefer an associated method to the signature, but ok.
    pub fn verify_sig(&self, sig: &StmSig<D>, msg: &[u8]) -> Result<(), VerificationFailure<D>> {
        let msgp = concat_avk_with_msg(&self.avk.to_commitment(), msg);
        let ev = sig.sigma.eval(&msgp, sig.index);

        if !ev_lt_phi(self.params.phi_f, ev, sig.stake, self.total_stake) {
            Err(VerificationFailure::LotteryLost)
        } else if self
            .avk
            .to_commitment()
            .check(&MTValue(sig.pk.vk, sig.stake).to_bytes(), &sig.path)
            .is_err()
        {
            Err(VerificationFailure::InvalidMerkleTree(sig.path.clone()))
        } else if sig.sigma.verify(&msgp, &sig.pk.vk).is_err() {
            Err(VerificationFailure::InvalidSignature(sig.sigma))
        } else {
            Ok(())
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
    ) -> Result<StmMultiSig<D>, AggregationFailure> {
        // todo: how come the dedup does not take the concatenated message
        // let msgp = concat_avk_with_msg(&self.avk.to_commitment(), msg);
        let mut unique_sigs = Vec::with_capacity(self.params.k.try_into().unwrap());
        for (_, sigs) in self.dedup_sigs_for_indices(msg, sigs)? {
            unique_sigs.push(sigs.clone())
        }

        Ok(StmMultiSig {
            signatures: unique_sigs,
        })
    }

    /// Verify an aggregation of signatures.
    ///
    /// The `From` bound on `Proof::Statement` allows `aggregate` to translate
    /// from the `Mithril` specific statement and witness types to their proof system-specific
    /// representations.
    pub fn verify_msig(
        &self,
        msig: &StmMultiSig<D>,
        msg: &[u8],
    ) -> Result<(), MithrilWitnessError<D>> {
        StmVerifier::new(self.avk.to_commitment(), self.params, self.total_stake)
            .verify_msig(msg, msig)
    }

    /// Given a slice of `indices` and one of `sigs`, this functions selects a single valid signature
    /// per index. In case of conflict (having several signatures for the same index) it selects the
    /// smallest signature (i.e. takes the signature with the smallest scalar). The function only
    /// selects `self.k` signatures. If there is no sufficient, then returns an error.
    fn dedup_sigs_for_indices<'a>(
        &self,
        msg: &[u8],
        sigs: &'a [StmSig<D>],
    ) -> Result<impl IntoIterator<Item = (&'a Index, &'a StmSig<D>)>, AggregationFailure> {
        let mut sigs_by_index: HashMap<&Index, &StmSig<D>> = HashMap::new();
        let mut count = 0;
        for sig in sigs {
            if self.verify_sig(sig, msg).is_err() {
                continue;
            }
            if let Some(old_sig) = sigs_by_index.get(&sig.index) {
                if sig.sigma < old_sig.sigma {
                    sigs_by_index.insert(&sig.index, sig);
                }
            } else {
                count += 1;
                sigs_by_index.insert(&sig.index, sig);
            }

            if count == self.params.k {
                return Ok(sigs_by_index.into_iter());
            }
        }
        Err(AggregationFailure::NotEnoughSignatures(
            count,
            self.params.k,
        ))
    }
}

#[cfg(feature = "num-integer-backend")]
/// Checks that ev is successful in the lottery. In particular, it compares the output of `phi`
/// (a real) to the output of `ev` (a hash).  It uses the same technique used in the
/// [Cardano ledger](https://github.com/input-output-hk/cardano-ledger/). In particular,
/// `ev` is a natural in `[0,2^512]`, while `phi` is a floating point in `[0, 1]`, and so what
/// this check does is verify whether `p < 1 - (1 - phi_f)^w`, with `p = ev / 2^512`.
///
/// The calculation is done using the following optimization:
///
/// let `q = 1 / (1 - p)` and `c = ln(1 - phi_f)`
///
/// then          `p < 1 - (1 - phi_f)^w`
/// `<=> 1 / (1 - p) < exp(-w * c)`
/// `<=> q           < exp(-w * c)`
///
/// This can be computed using the taylor expansion. Using error estimation, we can do
/// an early stop, once we know that the result is either above or below.  We iterate 1000
/// times. If no conclusive result has been reached, we return false.
///
/// Note that         1             1               evMax
///             q = ----- = ------------------ = -------------
///                 1 - p    1 - (ev / evMax)    (evMax - ev)
///
/// Used to determine winning lottery tickets.
pub fn ev_lt_phi(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
    // If phi_f = 1, then we automatically break with true
    if (phi_f - 1.0).abs() < f64::EPSILON {
        return true;
    }

    let ev_max = BigInt::from(2u8).pow(512);
    let ev = BigInt::from_bytes_le(Sign::Plus, &ev);
    let q = Ratio::new_raw(ev_max.clone(), ev_max - ev);

    let c =
        Ratio::from_float((1.0 - phi_f).ln()).expect("Only fails if the float is infinite or NaN.");
    let w = Ratio::new_raw(BigInt::from(stake), BigInt::from(total_stake));
    let x = (w * c).neg();
    // Now we compute a taylor function that breaks when the result is known.
    taylor_comparison(1000, q, x)
}

#[cfg(feature = "num-integer-backend")]
/// Checks if cmp < exp(x). Uses error approximation for an early stop. Whenever the value being
/// compared, `cmp`, is smaller (or greater) than the current approximation minus an `error_term`
/// (plus an `error_term` respectively), then we stop approximating. The choice of the `error_term`
/// is specific to our use case, and this function should not be used in other contexts without
/// reconsidering the `error_term`. As a conservative value of the `error_term` we choose
/// `new_x * M`, where `new_x` is the next term of the taylor expansion, and `M` is the largest
/// value of `x` in a reasonable range. Note that `x >= 0`, given that `x = - w * c`, with
/// `0 <= w <= 1` and `c < 0`, as `c` is defined as `c = ln(1.0 - phi_f)` with `phi_f \in (0,1)`.
/// Therefore, a good integral bound is the maximum value that `|ln(1.0 - phi_f)|` can take with
/// `phi_f \in [0, 0.95]` (if we expect to have `phi_f > 0.95` this bound should be extended),
/// which is `3`. Hence, we set `M = 3`.
fn taylor_comparison(bound: usize, cmp: Ratio<BigInt>, x: Ratio<BigInt>) -> bool {
    let mut new_x = x.clone();
    let mut phi: Ratio<BigInt> = One::one();
    let mut divisor: BigInt = One::one();
    for _ in 0..bound {
        phi += new_x.clone();

        divisor += 1;
        new_x = (new_x.clone() * x.clone()) / divisor.clone();
        let error_term = new_x.clone().abs() * BigInt::from(3); // new_x * M

        if cmp > (phi.clone() + error_term.clone()) {
            return false;
        } else if cmp < phi.clone() - error_term.clone() {
            return true;
        }
    }
    false
}

#[cfg(not(feature = "num-integer-backend"))]
/// The crate `rug` has sufficient optimizations to not require a taylor approximation with early
/// stop. The difference between the current implementation and the one using the optimization
/// above is around 10% faster. We perform the computations with 117 significant bits of
/// precision, since this is enough to represent the fraction of a single lovelace. We have that
/// 1e6 lovelace equals 1 ada, and there is 45 billion ada in circulation. Meaning there are
/// 4.5e16 lovelace, so 1e-17 is sufficient to represent fractions of the stake distribution. In
/// order to keep the error in the 1e-17 range, we need to carry out the computations with 34
/// decimal digits (in order to represent the 4.5e16 ada without any rounding errors, we need
/// double that precision).
pub fn ev_lt_phi(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
    use rug::{integer::Order, ops::Pow, Float};

    // If phi_f = 1, then we automatically break with true
    if (phi_f - 1.0).abs() < f64::EPSILON {
        return true;
    }
    let ev = rug::Integer::from_digits(&ev, Order::LsfLe);
    let ev_max: Float = Float::with_val(117, 2).pow(512);
    let q = ev / ev_max;

    let w = Float::with_val(117, stake) / Float::with_val(117, total_stake);
    let phi = Float::with_val(117, 1.0) - Float::with_val(117, 1.0 - phi_f).pow(w);

    q < phi
}

/// `StmVerifier` can verify `StmSig`s. `StmVerifiers` require les sinformation than
/// `StmClerks` or `StmSigner`s, as they do not require knowledge of the whole Merkle
/// Tree, but only the commitment.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StmVerifier<D>
where
    D: Clone + Digest + FixedOutput,
{
    avk_commitment: MerkleTreeCommitment<D>,
    params: StmParameters,
    total_stake: Stake,
}

impl<D> StmVerifier<D>
where
    D: Clone + Digest + FixedOutput,
{
    /// Generate a new StmVerifier. When creating a verifier from scratch, the top level application should validate
    /// that the avk_commitment, params, and total_stake are correct wrt some trusted state.
    pub fn new(
        avk_commitment: MerkleTreeCommitment<D>,
        params: StmParameters,
        total_stake: Stake,
    ) -> Self {
        Self {
            avk_commitment,
            params,
            total_stake,
        }
    }

    /// Verify an aggregated signature
    pub fn verify_msig(
        &self,
        msg: &[u8],
        sig: &StmMultiSig<D>,
    ) -> Result<(), MithrilWitnessError<D>> {
        sig.verify(msg, &self.avk_commitment, &self.params, self.total_stake)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key_reg::*;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use proptest::test_runner::{RngAlgorithm::ChaCha, TestRng};
    use rayon::prelude::*;
    use std::collections::{HashMap, HashSet};

    use num_bigint::{BigInt, Sign};
    use num_rational::Ratio;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    type Sig = StmMultiSig<D>;
    type D = blake2::Blake2b;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSigner<D>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSigner<D>> {
        let parties = stake
            .into_iter()
            .enumerate()
            .map(|(index, stake)| (index as u64, stake))
            .collect::<Vec<_>>();
        let mut kr = KeyReg::new(&parties);
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = ChaCha20Rng::from_seed(trng.gen());
        // The needless_collect lint is not correct here
        #[allow(clippy::needless_collect)]
        let ps = parties
            .into_iter()
            .map(|(pid, stake)| {
                let p = StmInitializer::setup(params, pid, stake, &mut rng);
                kr.register(p.party_id(), p.verification_key()).unwrap();
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

    fn find_signatures(m: u64, msg: &[u8], ps: &[StmSigner<D>], is: &[usize]) -> Vec<StmSig<D>> {
        let indices: Vec<_> = (0..m).collect();
        let res = indices
            .par_iter()
            .flat_map(|ix| {
                let mut sigs = Vec::new();
                for i in is {
                    if let Some(sig) = ps[*i].sign(msg, *ix) {
                        sigs.push(sig);
                    }
                }
                sigs
            })
            .collect();

        res
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that `dedup_sigs_for_indices` only takes valid signatures.
        fn test_dedup(msg in any::<[u8; 16]>(), misbehaving_parties in 2usize..5) {
            let nparties = 10usize;
            let false_msg = [1u8; 20];
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p);
            let mut sigs = Vec::with_capacity(nparties);
            let mut ixs = Vec::with_capacity(nparties);


            for index in 0..misbehaving_parties {
                if let Some(sig) = p.sign(&false_msg, index as u64) {
                    sigs.push(sig);
                    ixs.push(index as u64);
                }
            }

            for index in misbehaving_parties..nparties {
                if let Some(sig) = p.sign(&msg, index as u64) {
                    sigs.push(sig);
                    ixs.push(index as u64);
                }
            }

            for (_, passed_sigs) in clerk.dedup_sigs_for_indices(&msg, &sigs).unwrap() {
                assert!(clerk.verify_sig(passed_sigs, &msg).is_ok());
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


            for index in 1..nparties {
                if let Some(sig) = p.sign(&msg, index as u64) {
                    assert!(clerk.verify_sig(&sig, &msg).is_ok());
                }
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that when a quorum is found, the aggregate signature can be verified by an `StmClerk` or by
        /// an `StmVerifier`.
        fn test_aggregate_sig(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m, k, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(m, &msg, &ps, &all_ps);

            let msig = clerk.aggregate(&sigs, &msg);
            let verifier = StmVerifier::new(clerk.avk.to_commitment(), params, clerk.total_stake);

            match msig {
                Ok(aggr) => {
                    clerk.verify_msig(&aggr, &msg).unwrap();
                    // assert!(clerk.verify_msig(&aggr, &msg).is_ok());
                    assert!(verifier.verify_msig(&msg, &aggr).is_ok());
                }
                Err(AggregationFailure::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k || k == params.k),
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        fn test_sig_serialize_deserialize(msg in any::<[u8;16]>()) {
            let nparties = 2;
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p);

            for index in 0..nparties {
                if let Some(sig) = p.sign(&msg, index as u64) {
                    let bytes = sig.to_bytes();
                    let sig_deser = StmSig::<D>::from_bytes(&bytes).unwrap();
                    assert!(clerk.verify_sig(&sig_deser, &msg).is_ok());
                }
            }
        }

        #[test]
        fn test_multisig_serialize_deserialize(nparties in 2_usize..10,
                                          msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 10, k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(10, &msg, &ps, &all_ps);
            let msig = clerk.aggregate(&sigs, &msg);
            if let Ok(aggr) = msig {
                    let bytes: Vec<u8> = aggr.to_bytes();
                    let aggr2 = StmMultiSig::from_bytes(&bytes).unwrap();
                    assert!(clerk.verify_msig(&aggr2, &msg).is_ok());
            }
        }
    }

    /// Pick N between min and max, and then
    /// generate a vector of N stakes summing to N*tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake*N
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

            let sigs = find_signatures(params.m,
                                              &msg,
                                              &ps,
                                              &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

            let clerk = StmClerk::from_signer(&ps[0]);

            let msig = clerk.aggregate(&sigs, &msg);
            match msig {
                Err(AggregationFailure::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k && params.k == k),
                _ =>
                    unreachable!(),
            }
        }
    }

    #[derive(Debug)]
    struct ProofTest {
        n: usize,
        msig: Result<Sig, AggregationFailure>,
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
                let sigs = find_signatures(params.m, &msg, &ps, &all_ps);

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
                assert!(tc.clerk.verify_msig(&aggr, &tc.msg).is_err())
            }
            Err(e) => unreachable!("Reached an unexpected error: {:?}", e),
        }
    }

    proptest! {
        // Each of the tests below corresponds to falsifying a conjunct in the
        // defintion of the proved relation between statement & witness as
        // defined in the Mithril protocol
        #[test]
        fn test_invalid_proof_quorum(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |_aggr, clerk, _msg| {
                clerk.params.k += 1;
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
                    sig.index %= clerk.params.k - 1
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10), i in any::<usize>()) {
            let n = tc.n;
            with_proof_mod(tc, |aggr, clerk, _msg| {
                let pi = i % clerk.params.k as usize;
                aggr.signatures[pi].path.index = (aggr.signatures[pi].party + 1) as usize % n;
            })
        }
    }

    // Implementation of `ev_lt_phi` without approximation. We only get the precision of f64 here.
    fn simple_ev_lt_phi(phi_f: f64, ev: [u8; 64], stake: Stake, total_stake: Stake) -> bool {
        let ev_max = BigInt::from(2u8).pow(512);
        let ev = BigInt::from_bytes_le(Sign::Plus, &ev);
        let q = Ratio::new_raw(ev, ev_max);

        let w = stake as f64 / total_stake as f64;
        let phi = Ratio::from_float(1.0 - (1.0 - phi_f).powf(w)).unwrap();
        q < phi
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Checking the ev_lt_phi function.
        fn test_precision_approximation(
            phi_f in 0.01..0.5f64,
            ev_1 in any::<[u8; 32]>(),
            ev_2 in any::<[u8; 32]>(),
            total_stake in 100_000_000..1_000_000_000u64,
            stake in 1_000_000..50_000_000u64
        ) {
            let mut ev = [0u8; 64];
            ev.copy_from_slice(&[&ev_1[..], &ev_2[..]].concat());

            let quick_result = simple_ev_lt_phi(phi_f, ev, stake, total_stake);
            let result = ev_lt_phi(phi_f, ev, stake, total_stake);
            assert_eq!(quick_result, result);
        }

        #[cfg(feature = "num-integer-backend")]
        #[test]
        /// Checking the early break of Taylor compuation
        fn early_break_taylor(
            x in -0.9..0.9f64,
        ) {
            let exponential = num_traits::float::Float::exp(x);
            let cmp_n = Ratio::from_float(exponential - 2e-10_f64).unwrap();
            let cmp_p = Ratio::from_float(exponential + 2e-10_f64).unwrap();
            assert!(taylor_comparison(1000, cmp_n, Ratio::from_float(x).unwrap()));
            assert!(!taylor_comparison(1000, cmp_p, Ratio::from_float(x).unwrap()));
        }
    }
}
