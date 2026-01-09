#![doc = include_str!("../README.md")]
//! Implementation of Stake-based Threshold Multisignatures
//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.
//! See figure 6 of [the paper](https://eprint.iacr.org/2021/916) for most of the
//! protocol.
//!
//! What follows is a simple example showing the usage of STM.
//!
//! ```rust
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use blake2::{Blake2b, digest::consts::U32};
//! use rand_chacha::ChaCha20Rng;
//! use rand_core::{RngCore, SeedableRng};
//! use rayon::prelude::*; // We use par_iter to speed things up
//!
//! use mithril_stm::{
//!    AggregateSignatureType, AggregationError, Clerk, Initializer, KeyRegistration, Parameters,
//!    RegistrationEntry, Signer, SingleSignature, MithrilMembershipDigest,
//! };
//!
//! let nparties = 4; // Use a small number of parties for this example
//! type D = MithrilMembershipDigest; // Setting the hash function for convenience
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
//! let params = Parameters {
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
//! let mut key_reg = KeyRegistration::initialize();
//!
//! // For each party, crate a Initializer.
//! // This struct can create keys for the party.
//! let mut ps: Vec<Initializer> = Vec::with_capacity(nparties);
//! for stake in stakes {
//!     // Create keys for this party
//!     let p = Initializer::new(params, stake, &mut rng);
//!     // Register keys with the KeyRegistration service
//!     let entry = RegistrationEntry::new(
//!         p.get_verification_key_proof_of_possession(),
//!         p.stake,
//!     )
//!     .unwrap();
//!     key_reg.register_by_entry(&entry).unwrap();
//!     ps.push(p);
//! }
//!
//! // Close the key registration.
//! let closed_reg = key_reg.close_registration();
//!
//! // Finalize the Initializer and turn it into a Signer, which can execute the
//! // rest of the protocol.
//! let ps = ps
//!     .into_par_iter()
//!     .map(|p| p.try_create_signer(&closed_reg).unwrap())
//!     .collect::<Vec<Signer<D>>>();
//!
//! /////////////////////
//! // operation phase //
//! /////////////////////
//!
//! // Next, each party tries to sign the message for each index available.
//! // We collect the successful signatures into a vec.
//! let sigs = ps
//!     .par_iter()
//!     .filter_map(|p| p.create_single_signature(&msg).ok())
//!     .collect::<Vec<SingleSignature>>();
//!
//! // Clerk can aggregate and verify signatures.
//! let clerk = Clerk::new_clerk_from_signer(&ps[0]);
//!
//! // Aggregate and verify the signatures
//! let msig = clerk.aggregate_signatures_with_type(&sigs, &msg, AggregateSignatureType::Concatenation);
//! match msig {
//!     Ok(aggr) => {
//!         println!("Aggregate ok");
//!         assert!(aggr
//!             .verify(&msg, &clerk.compute_aggregate_verification_key(), &params)
//!             .is_ok());
//!     }
//!     Err(error) => assert!(
//!         matches!(
//!             error.downcast_ref::<AggregationError>(),
//!             Some(AggregationError::NotEnoughSignatures { .. })
//!         ),
//!         "Unexpected error: {error}"
//!     ),
//! }
//! # Ok(())
//! # }
//! ```

#[cfg(feature = "future_snark")]
pub mod circuits;
#[cfg(feature = "future_snark")]
mod hash;
mod membership_commitment;
mod proof_system;
mod protocol;
mod signature_scheme;

pub use protocol::{
    AggregateSignature, AggregateSignatureError, AggregateSignatureType, AggregateVerificationKey,
    AggregationError, Clerk, ClosedKeyRegistration, Initializer, KeyRegistration, Parameters,
    RegisterError, RegistrationEntry, RegistrationEntryForConcatenation, SignatureError, Signer,
    SingleSignature, SingleSignatureWithRegisteredParty, VerificationKeyForConcatenation,
    VerificationKeyProofOfPossessionForConcatenation,
};
pub use signature_scheme::BlsSignatureError;

#[cfg(feature = "benchmark-internals")]
pub use signature_scheme::{
    BlsProofOfPossession, BlsSignature, BlsSigningKey, BlsVerificationKey,
    BlsVerificationKeyProofOfPossession,
};

#[cfg(all(feature = "benchmark-internals", feature = "future_snark"))]
pub use signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey, UniqueSchnorrSignature};

use blake2::{Blake2b, digest::consts::U32};
use digest::{Digest, FixedOutput};
use std::fmt::Debug;

#[cfg(feature = "future_snark")]
use hash::poseidon::MidnightPoseidonDigest;

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type LotteryIndex = u64;

/// Index of the signer in the key registration
pub type SignerIndex = u64;

/// Mithril-stm error type
pub type StmError = anyhow::Error;

/// Mithril-stm result type
pub type StmResult<T> = anyhow::Result<T, StmError>;

/// Trait defining the different hash types for different proof systems.
pub trait MembershipDigest: Clone {
    type ConcatenationHash: Digest + FixedOutput + Clone + Debug + Send + Sync;
    #[cfg(feature = "future_snark")]
    type SnarkHash: Digest + FixedOutput + Clone + Debug + Send + Sync;
}

/// Default Mithril Membership Digest
#[derive(Clone, Debug, Default)]
pub struct MithrilMembershipDigest {}

/// Default implementation of MembershipDigest for Mithril
/// TODO: `SnarkHash` will be changed with Poseidon. For now, we use `Blake2b<U64>` (`U64` is set
/// for having something different than the `ConcatenationHash`) as a placeholder.
impl MembershipDigest for MithrilMembershipDigest {
    type ConcatenationHash = Blake2b<U32>;
    #[cfg(feature = "future_snark")]
    type SnarkHash = MidnightPoseidonDigest;
}
