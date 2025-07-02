use blst::min_sig::SecretKey as BlstSk;
use rand_core::{CryptoRng, RngCore};

use crate::bls_multi_signature::signature::BlsSignature;
use crate::error::{blst_err_to_mithril, MultiSignatureError};

/// MultiSig secret key, which is a wrapper over the BlstSk type from the blst
/// library.
#[derive(Debug, Clone)]
pub struct BlsSigningKey(pub BlstSk);

impl BlsSigningKey {
    /// Generate a secret key
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        BlsSigningKey(
            BlstSk::key_gen(&ikm, &[])
                .expect("Error occurs when the length of ikm < 32. This will not happen here."),
        )
    }

    /// Sign a message with the given secret key
    pub fn sign(&self, msg: &[u8]) -> BlsSignature {
        BlsSignature(self.0.sign(msg, &[], &[]))
    }

    /// Convert the secret key into byte string.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SigningKey`.
    ///
    /// # Error
    /// Fails if the byte string represents a scalar larger than the group order.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let bytes = bytes.get(..32).ok_or(MultiSignatureError::SerializationError)?;
        match BlstSk::from_bytes(bytes) {
            Ok(sk) => Ok(Self(sk)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
    }

    pub(crate) fn to_blst_sk(&self) -> BlstSk {
        self.0.clone()
    }
}
