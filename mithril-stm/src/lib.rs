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
//!    Signer, SingleSignature,
//! };
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
//! let mut key_reg = KeyRegistration::init();
//!
//! // For each party, crate a Initializer.
//! // This struct can create keys for the party.
//! let mut ps: Vec<Initializer> = Vec::with_capacity(nparties);
//! for stake in stakes {
//!     // Create keys for this party
//!     let p = Initializer::new(params, stake, &mut rng);
//!     // Register keys with the KeyRegistration service
//!     key_reg
//!         .register(p.stake, p.verification_key())
//!         .unwrap();
//!     ps.push(p);
//! }
//!
//! // Close the key registration.
//! let closed_reg = key_reg.close();
//!
//! // Finalize the Initializer and turn it into a Signer, which can execute the
//! // rest of the protocol.
//! let ps = ps
//!     .into_par_iter()
//!     .map(|p| p.new_signer(closed_reg.clone()).unwrap())
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
//!     .filter_map(|p| {
//!         return p.sign(&msg);
//!     })
//!     .collect::<Vec<SingleSignature>>();
//!
//! // Clerk can aggregate and verify signatures.
//! let clerk = Clerk::from_signer(&ps[0]);
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

mod aggregate_signature;
mod bls_multi_signature;
mod eligibility_check;
mod error;
mod key_registration;
mod merkle_tree;
mod parameters;
mod participant;
#[cfg(feature = "future_snark")]
mod schnorr_signature;
mod single_signature;

pub use aggregate_signature::{
    AggregateSignature, AggregateSignatureType, AggregateVerificationKey, BasicVerifier, Clerk,
};
pub use error::{
    AggregateSignatureError, AggregationError, MultiSignatureError, RegisterError, SignatureError,
};
pub use key_registration::{ClosedKeyRegistration, KeyRegistration};
pub use parameters::Parameters;
pub use participant::{Initializer, Signer, VerificationKey, VerificationKeyProofOfPossession};
pub use single_signature::{SingleSignature, SingleSignatureWithRegisteredParty};

#[cfg(feature = "benchmark-internals")]
pub use bls_multi_signature::{
    BlsProofOfPossession, BlsSignature, BlsSigningKey, BlsVerificationKey,
    BlsVerificationKeyProofOfPossession,
};

#[cfg(feature = "future_snark")]
pub use schnorr_signature::{SchnorrSignature, SchnorrSigningKey, SchnorrVerificationKey};

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// Mithril-stm error type
pub type StmError = anyhow::Error;

/// Mithril-stm result type
pub type StmResult<T> = anyhow::Result<T, StmError>;

// Aliases
#[deprecated(since = "0.5.0", note = "Use `AggregateSignature` instead")]
pub use aggregate_signature::AggregateSignature as StmAggrSig;

#[deprecated(since = "0.5.0", note = "Use `AggregateVerificationKey` instead")]
pub use aggregate_signature::AggregateVerificationKey as StmAggrVerificationKey;

#[deprecated(since = "0.5.0", note = "Use `Clerk` instead")]
pub use aggregate_signature::Clerk as StmClerk;

#[deprecated(since = "0.5.0", note = "Use `ClosedKeyRegistration` instead")]
pub use key_registration::ClosedKeyRegistration as ClosedKeyReg;

#[deprecated(since = "0.5.0", note = "Use `KeyRegistration` instead")]
pub use key_registration::KeyRegistration as KeyReg;

#[deprecated(since = "0.5.0", note = "Use `Parameters` instead")]
pub use parameters::Parameters as StmParameters;

#[deprecated(since = "0.5.0", note = "Use `Initializer` instead")]
pub use participant::Initializer as StmInitializer;

#[deprecated(since = "0.5.0", note = "Use `Signer` instead")]
pub use participant::Signer as StmSigner;

#[deprecated(since = "0.5.0", note = "Use `VerificationKey` instead")]
pub use participant::VerificationKey as StmVerificationKey;

#[deprecated(
    since = "0.5.0",
    note = "Use `VerificationKeyProofOfPossession` instead"
)]
pub use participant::VerificationKeyProofOfPossession as StmVerificationKeyPoP;

#[deprecated(since = "0.5.0", note = "Use `SingleSignature` instead")]
pub use single_signature::SingleSignature as StmSig;

#[deprecated(since = "0.5.0", note = "Use `BasicVerifier` instead")]
pub use aggregate_signature::BasicVerifier as CoreVerifier;

#[deprecated(
    since = "0.5.0",
    note = "Use `SingleSignatureWithRegisteredParty` instead"
)]
pub use single_signature::SingleSignatureWithRegisteredParty as StmSigRegParty;
