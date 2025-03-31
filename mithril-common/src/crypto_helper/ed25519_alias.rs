use crate::crypto_helper::ed25519::*;

pub mod genesis {
    use super::*;

    /// A protocol Genesis verifier secret key
    pub type ProtocolGenesisSecretKey = Ed25519SecretKey;
    /// A protocol Genesis verification key
    pub type ProtocolGenesisVerificationKey = Ed25519VerificationKey;
    /// A protocol Genesis signature
    pub type ProtocolGenesisSignature = Ed25519Signature;
    /// A protocol Genesis Signer that is responsible for signing the
    /// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
    pub type ProtocolGenesisSigner = Ed25519Signer;
    /// A protocol Genesis Verifier that is responsible for verifying the
    /// [Genesis Certificate](https://mithril.network/doc/mithril/mithril-protocol/certificates#the-certificate-chain-design)
    pub type ProtocolGenesisVerifier = Ed25519Verifier;
    /// [ProtocolGenesisSigner] and [ProtocolGenesisVerifier] related errors.
    pub type ProtocolGenesisError = Ed25519VerifierError;
}

pub mod era {
    use super::*;

    /// Era markers verifier secret key
    pub type EraMarkersVerifierSecretKey = Ed25519SecretKey;
    /// Era markers verifier verification key
    pub type EraMarkersVerifierVerificationKey = Ed25519VerificationKey;
    /// Era markers verifier signature
    pub type EraMarkersVerifierSignature = Ed25519Signature;
    /// A cryptographic signer that is responsible for signing the EraMarkers
    pub type EraMarkersSigner = Ed25519Signer;
    /// An era markers verifier that checks the authenticity of era markers stored on the chain
    pub type EraMarkersVerifier = Ed25519Verifier;
    /// [EraMarkersSigner] and [EraMarkersVerifier] related errors.
    pub type EraMarkersVerifierError = Ed25519VerifierError;
}

pub mod manifest {
    use super::*;

    /// Manifest verifier secret key
    pub type ManifestVerifierSecretKey = Ed25519SecretKey;
    /// Manifest verifier verification key
    pub type ManifestVerifierVerificationKey = Ed25519VerificationKey;
    /// Manifest signature
    pub type ManifestSignature = Ed25519Signature;
    /// A cryptographic signer that is responsible for signing the Manifest
    pub type ManifestSigner = Ed25519Signer;
    /// A manifest verifier that checks the authenticity of a manifest
    pub type ManifestVerifier = Ed25519Verifier;
    /// [ManifestSigner] and [ManifestVerifier] related errors.
    pub type ManifestVerifierError = Ed25519VerifierError;
}
