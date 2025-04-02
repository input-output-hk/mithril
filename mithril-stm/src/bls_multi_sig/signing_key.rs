
use crate::error::{blst_err_to_mithril, MultiSignatureError};
use blst::min_sig::{
    AggregatePublicKey, AggregateSignature, PublicKey as BlstVk, SecretKey as BlstSk,
    Signature as BlstSig,
};
use blst::{blst_p1, blst_p2, p1_affines, p2_affines, BLST_ERROR};
use rand_core::{CryptoRng, RngCore};
use crate::bls_multi_sig::signature::Signature;

/// MultiSig secret key, which is a wrapper over the BlstSk type from the blst
/// library.
#[derive(Debug, Clone)]
pub struct SigningKey(BlstSk);

impl SigningKey {
    /// Generate a secret key
    pub fn gen(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        SigningKey(
            BlstSk::key_gen(&ikm, &[])
                .expect("Error occurs when the length of ikm < 32. This will not happen here."),
        )
    }

    /// Sign a message with the given secret key
    pub fn sign(&self, msg: &[u8]) -> Signature {
        Signature(self.0.sign(msg, &[], &[]))
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
        match BlstSk::from_bytes(&bytes[..32]) {
            Ok(sk) => Ok(Self(sk)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
    }

    pub(crate) fn to_blst_sk(&self) -> BlstSk {
        self.0
    }
}
