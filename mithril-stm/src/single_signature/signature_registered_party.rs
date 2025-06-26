use blake2::digest::{Digest, FixedOutput};
use serde::{ser::SerializeTuple, Deserialize, Serialize, Serializer};

use crate::key_reg::RegParty;
use crate::{StmSig, StmSignatureError};

/// Signature with its registered party.
#[derive(Debug, Clone, Hash, Deserialize, Eq, PartialEq, Ord, PartialOrd)]
pub struct StmSigRegParty {
    /// Stm signature
    pub sig: StmSig,
    /// Registered party
    pub reg_party: RegParty,
}

impl StmSigRegParty {
    /// Convert StmSigRegParty to bytes
    /// # Layout
    /// * RegParty
    /// * Signature
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&self.reg_party.to_bytes());
        out.extend_from_slice(&self.sig.to_bytes());

        out
    }
    ///Extract a `StmSigRegParty` from a byte slice.
    pub fn from_bytes<D: Digest + Clone + FixedOutput>(
        bytes: &[u8],
    ) -> Result<StmSigRegParty, StmSignatureError> {
        let reg_party =
            RegParty::from_bytes(bytes.get(0..104).ok_or(StmSignatureError::SerializationError)?)?;
        let sig = StmSig::from_bytes::<D>(
            bytes.get(104..).ok_or(StmSignatureError::SerializationError)?,
        )?;

        Ok(StmSigRegParty { sig, reg_party })
    }
}

impl Serialize for StmSigRegParty {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tuple = serializer.serialize_tuple(2)?;
        tuple.serialize_element(&self.sig)?;
        tuple.serialize_element(&self.reg_party)?;
        tuple.end()
    }
}
