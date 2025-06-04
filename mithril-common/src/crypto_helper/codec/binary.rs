use crate::StdResult;

/// Traits for serializing to bytes
pub trait IntoBytes {
    /// Convert into a bytes vector.
    fn into_bytes(&self) -> Vec<u8>;
}

/// Traits for deserializing from bytes
pub trait TryFromBytes: Sized {
    /// Try to convert from a bytes slice.
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self>;
}

mod binary_mithril_stm {
    use anyhow::anyhow;
    use blake2::Blake2b;

    use digest::consts::U32;
    use mithril_stm::{
        ProofOfPossession, Signature, SigningKey, StmAggrSig, StmAggrVerificationKey,
        StmInitializer, StmParameters, StmSig, StmSigRegParty, VerificationKey, VerificationKeyPoP,
    };

    use crate::crypto_helper::{key_decode_hex, key_encode_hex};

    use super::*;

    type D = Blake2b<U32>;

    impl IntoBytes for StmParameters {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmParameters {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmSig {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmSig {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes::<D>(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmSigRegParty {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmSigRegParty {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes::<D>(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmAggrSig<D> {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmAggrSig<D> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| anyhow!("{e}"))
        }
    }

    impl IntoBytes for ProofOfPossession {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for ProofOfPossession {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for Signature {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for Signature {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for SigningKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for SigningKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for VerificationKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for VerificationKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for VerificationKeyPoP {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for VerificationKeyPoP {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmAggrVerificationKey<D> {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl TryFromBytes for StmAggrVerificationKey<D> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap()).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmInitializer {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmInitializer {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }
}

mod binary_ed25519 {
    use ed25519_dalek::{Signature, SigningKey, VerifyingKey};

    use super::*;

    impl IntoBytes for Signature {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for Signature {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::try_from(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for SigningKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for SigningKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::try_from(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for VerifyingKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for VerifyingKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::try_from(bytes).map_err(|e| e.into())
        }
    }
}

mod binary_kes_sig {
    use anyhow::anyhow;
    use kes_summed_ed25519::kes::Sum6KesSig;

    use super::*;

    impl IntoBytes for Sum6KesSig {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for Sum6KesSig {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| anyhow!(format!("{e:?}")))
        }
    }
}

mod binary_opcert {
    use crate::crypto_helper::{key_decode_hex, key_encode_hex, OpCert};

    use super::*;

    impl IntoBytes for OpCert {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl TryFromBytes for OpCert {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap()).map_err(|e| e.into())
        }
    }
}

mod binary_mk_proof {
    use serde::{de::DeserializeOwned, Serialize};

    use crate::crypto_helper::{key_decode_hex, key_encode_hex, MKMapKey, MKMapProof, MKProof};

    use super::*;

    impl<T: MKMapKey + Serialize> IntoBytes for MKMapProof<T> {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl<T: MKMapKey + DeserializeOwned> TryFromBytes for MKMapProof<T> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap()).map_err(|e| e.into())
        }
    }

    impl IntoBytes for MKProof {
        fn into_bytes(&self) -> Vec<u8> {
            // TODO: Use a more efficient serialization method
            key_encode_hex(self).unwrap().into_bytes()
        }
    }

    impl TryFromBytes for MKProof {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            // TODO: Use a more efficient deserialization method
            key_decode_hex(std::str::from_utf8(bytes).unwrap()).map_err(|e| e.into())
        }
    }
}
