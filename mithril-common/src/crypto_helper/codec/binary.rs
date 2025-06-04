use hex::{FromHex, ToHex};

use crate::StdResult;

/// Traits for serializing to bytes
pub trait IntoBytes {
    /// Convert into a bytes vector.
    fn into_bytes(&self) -> Vec<u8>;

    /// Convert to hex bytes representation
    fn into_bytes_hex(&self) -> String {
        self.into_bytes().encode_hex::<String>()
    }
}

/// Traits for deserializing from bytes
pub trait TryFromBytes: Sized {
    /// Try to convert from a bytes slice.
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self>;

    /// Try to convert from hex string encoded bytes.
    fn try_from_bytes_hex(hex_string: &str) -> StdResult<Self> {
        let bytes = Vec::from_hex(hex_string).map_err(|e| {
            anyhow::anyhow!(
                "Could not deserialize binary from hex string: {}",
                e.to_string()
            )
        })?;

        Self::try_from_bytes(&bytes)
    }
}

mod binary_mithril_stm {
    use anyhow::anyhow;
    use blake2::Blake2b;

    use digest::consts::U32;
    use mithril_stm::{
        StmAggrSig, StmAggrVerificationKey, StmInitializer, StmParameters, StmSig, StmSigRegParty,
        StmVerificationKey, StmVerificationKeyPoP,
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

    impl IntoBytes for StmVerificationKey {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmVerificationKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl IntoBytes for StmVerificationKeyPoP {
        fn into_bytes(&self) -> Vec<u8> {
            self.to_bytes().to_vec()
        }
    }

    impl TryFromBytes for StmVerificationKeyPoP {
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
