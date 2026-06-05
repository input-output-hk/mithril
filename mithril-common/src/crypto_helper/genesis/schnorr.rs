//! Aliases for the SNARK-friendly genesis primitives (Schnorr over Jubjub).
//!
//! These types name the genesis usage explicitly and reuse the cryptographic primitives
//! defined in [schnorr](crate::crypto_helper::schnorr), mirroring the legacy ed25519-genesis
//! aliases (see [ed25519](super::ed25519)).

use crate::crypto_helper::schnorr::*;

pub use crate::crypto_helper::schnorr::{
    PREIMAGE_SIZE, schnorr_signature_from_hex, schnorr_signature_to_hex, sha256_digest,
    signed_message_from_digest,
};

/// A protocol Schnorr Genesis verifier secret key
pub type GenesisSchnorrSecretKey = SchnorrSecretKey;
/// A protocol Schnorr Genesis verification key
pub type GenesisSchnorrVerificationKey = SchnorrVerificationKey;
/// A protocol Schnorr Genesis signature (standard Schnorr signature over Jubjub)
pub type GenesisSchnorrSignature = SchnorrSignature;
/// A protocol Schnorr Genesis Signer responsible for signing the genesis attestation
pub type GenesisSchnorrSigner = SchnorrSigner;
/// A protocol Schnorr Genesis Verifier responsible for verifying the genesis attestation
pub type GenesisSchnorrVerifier = SchnorrVerifier;
/// [GenesisSchnorrSigner] and [GenesisSchnorrVerifier] related errors.
pub type GenesisSchnorrError = SchnorrError;
