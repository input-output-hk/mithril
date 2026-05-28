//! Aliases for the legacy ed25519-proof genesis primitives (Ed25519).
//!
//! These types name the cryptographic domain explicitly to leave room for the SNARK-friendly
//! Schnorr family added under the Lagrange era (see [schnorr](super::schnorr)).

use crate::crypto_helper::ed25519::*;

/// A protocol Ed25519 Genesis verifier secret key
pub type GenesisEd25519SecretKey = Ed25519SecretKey;
/// A protocol Ed25519 Genesis verification key
pub type GenesisEd25519VerificationKey = Ed25519VerificationKey;
/// A protocol Ed25519 Genesis signature
pub type GenesisEd25519Signature = Ed25519Signature;
/// A protocol Ed25519 Genesis Signer that is responsible for signing the
/// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
pub type GenesisEd25519Signer = Ed25519Signer;
/// A protocol Ed25519 Genesis Verifier that is responsible for verifying the
/// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
pub type GenesisEd25519Verifier = Ed25519Verifier;
/// [GenesisEd25519Signer] and [GenesisEd25519Verifier] related errors.
pub type GenesisEd25519Error = Ed25519VerifierError;
