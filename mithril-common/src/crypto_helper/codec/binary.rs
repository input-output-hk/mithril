/// Traits for serializing to bytes
pub trait IntoBytes {
    /// Convert into a bytes vector.
    fn into_bytes(&self) -> Vec<u8>;
}

/// Traits for deserializing from bytes
pub trait TryFromBytes<D, E>: Sized {
    /// Try to convert from a bytes slice.
    fn try_from_bytes(bytes: &[u8]) -> Result<Self, E>;
}

mod binary_mithril_stm {
    use blake2::digest::{Digest, FixedOutput};

    use mithril_stm::{
        MultiSignatureError, ProofOfPossession, RegisterError, Signature, SigningKey, StmAggrSig,
        StmAggrVerificationKey, StmAggregateSignatureError, StmInitializer, StmParameters, StmSig,
        StmSigRegParty, StmSignatureError, VerificationKey, VerificationKeyPoP,
    };

    use crate::crypto_helper::{key_decode_hex, key_encode_hex, CodecError};

    use super::*;

    impl IntoBytes for StmParameters {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), RegisterError> for StmParameters {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for StmSig {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl<D: Clone + Digest + FixedOutput> TryFromBytes<D, StmSignatureError> for StmSig {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, StmSignatureError> {
            Self::from_bytes::<D>(bytes)
        }
    }

    impl IntoBytes for StmSigRegParty {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl<D: Digest + Clone + FixedOutput> TryFromBytes<D, StmSignatureError> for StmSigRegParty {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, StmSignatureError> {
            Self::from_bytes::<D>(bytes)
        }
    }

    impl<D: Clone + Digest + FixedOutput + Send + Sync> IntoBytes for StmAggrSig<D> {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl<D: Clone + Digest + FixedOutput + Send + Sync>
        TryFromBytes<D, StmAggregateSignatureError<D>> for StmAggrSig<D>
    {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, StmAggregateSignatureError<D>> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for ProofOfPossession {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), MultiSignatureError> for ProofOfPossession {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for Signature {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), MultiSignatureError> for Signature {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for SigningKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), MultiSignatureError> for SigningKey {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for VerificationKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), MultiSignatureError> for VerificationKey {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
            Self::from_bytes(bytes)
        }
    }

    impl IntoBytes for VerificationKeyPoP {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), MultiSignatureError> for VerificationKeyPoP {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
            Self::from_bytes(bytes)
        }
    }

    impl<D: Clone + Digest + FixedOutput + Send + Sync> IntoBytes for StmAggrVerificationKey<D> {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl<D: Clone + Digest + FixedOutput + Send + Sync> TryFromBytes<D, CodecError>
        for StmAggrVerificationKey<D>
    {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, CodecError> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap())
        }
    }

    impl IntoBytes for StmInitializer {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), RegisterError> for StmInitializer {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
            Self::from_bytes(bytes)
        }
    }
}

mod binary_ed25519 {
    use ed25519_dalek::{ed25519::Error, Signature, SigningKey, VerifyingKey};

    use super::*;

    impl IntoBytes for Signature {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), Error> for Signature {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, Error> {
            Self::try_from(bytes)
        }
    }

    impl IntoBytes for SigningKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), Error> for SigningKey {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, Error> {
            Self::try_from(bytes)
        }
    }

    impl IntoBytes for VerifyingKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), Error> for VerifyingKey {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, Error> {
            Self::try_from(bytes)
        }
    }
}

mod binary_kes_sig {
    use anyhow::{anyhow, Error};
    use kes_summed_ed25519::kes::Sum6KesSig;

    use super::*;

    impl IntoBytes for Sum6KesSig {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes<(), Error> for Sum6KesSig {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, Error> {
            Self::from_bytes(bytes).map_err(|e| anyhow!(format!("{e:?}")))
        }
    }
}

mod binary_opcert {
    use crate::crypto_helper::{key_decode_hex, key_encode_hex, CodecError, OpCert};

    use super::*;

    impl IntoBytes for OpCert {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl TryFromBytes<(), CodecError> for OpCert {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, CodecError> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap())
        }
    }
}

mod binary_mkmap_proof {
    use serde::{de::DeserializeOwned, Serialize};

    use crate::crypto_helper::{key_decode_hex, key_encode_hex, CodecError, MKMapKey, MKMapProof};

    use super::*;

    impl<T: MKMapKey + Serialize> IntoBytes for MKMapProof<T> {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl<T: MKMapKey + DeserializeOwned> TryFromBytes<(), CodecError> for MKMapProof<T> {
        fn try_from_bytes(bytes: &[u8]) -> Result<Self, CodecError> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap())
        }
    }
}
