use hex::{FromHex, ToHex};

use crate::StdResult;

/// Traits for serializing to bytes
pub trait TryToBytes {
    /// Try to convert to a bytes vector.
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>>;

    /// Try to convert to hex bytes representation
    fn to_bytes_hex(&self) -> StdResult<String> {
        Ok(self.to_bytes_vec()?.encode_hex::<String>())
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
        Initializer, Parameters, SingleSignature, SingleSignatureWithRegisteredParty, StmAggrSig,
        StmAggrVerificationKey, StmVerificationKey, StmVerificationKeyPoP,
    };

    use super::*;

    type D = Blake2b<U32>;

    impl TryToBytes for Parameters {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for Parameters {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for SingleSignature {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for SingleSignature {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes::<D>(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for SingleSignatureWithRegisteredParty {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for SingleSignatureWithRegisteredParty {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes::<D>(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for StmAggrSig<D> {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for StmAggrSig<D> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| anyhow!("{e}"))
        }
    }

    impl TryToBytes for StmVerificationKey {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for StmVerificationKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for StmVerificationKeyPoP {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for StmVerificationKeyPoP {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for StmAggrVerificationKey<D> {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            bincode::serde::encode_to_vec(self, bincode::config::standard()).map_err(|e| e.into())
        }
    }

    impl TryFromBytes for StmAggrVerificationKey<D> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            let (res, _) = bincode::serde::decode_from_slice::<StmAggrVerificationKey<D>, _>(
                bytes,
                bincode::config::standard(),
            )
            .map_err(|e| anyhow!(e))?;

            Ok(res)
        }
    }

    impl TryToBytes for Initializer {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for Initializer {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| e.into())
        }
    }
}

mod binary_ed25519 {
    use ed25519_dalek::{Signature, SigningKey, VerifyingKey};

    use super::*;

    impl TryToBytes for Signature {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for Signature {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::try_from(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for SigningKey {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for SigningKey {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::try_from(bytes).map_err(|e| e.into())
        }
    }

    impl TryToBytes for VerifyingKey {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
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

    impl TryToBytes for Sum6KesSig {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            Ok(self.to_bytes().to_vec())
        }
    }

    impl TryFromBytes for Sum6KesSig {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes).map_err(|e| anyhow!(format!("{e:?}")))
        }
    }
}

mod binary_opcert {
    use crate::crypto_helper::{OpCert, SerDeShelleyFileFormat};
    use anyhow::anyhow;

    use super::*;

    impl TryToBytes for OpCert {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            self.to_cbor_bytes().map_err(|e| anyhow!(format!("{e:?}")))
        }
    }

    impl TryFromBytes for OpCert {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_cbor_bytes(bytes).map_err(|e| anyhow!(format!("{e:?}")))
        }
    }
}

mod binary_mk_proof {
    use serde::{Deserialize, Serialize};

    use crate::crypto_helper::{MKMapKey, MKMapProof, MKProof};

    use super::*;

    impl<T: MKMapKey + Serialize + for<'de> Deserialize<'de>> TryToBytes for MKMapProof<T> {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            self.to_bytes()
        }
    }

    impl<T: MKMapKey + Serialize + for<'de> Deserialize<'de>> TryFromBytes for MKMapProof<T> {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes)
        }
    }

    impl TryToBytes for MKProof {
        fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
            self.to_bytes()
        }
    }

    impl TryFromBytes for MKProof {
        fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
            Self::from_bytes(bytes)
        }
    }
}
