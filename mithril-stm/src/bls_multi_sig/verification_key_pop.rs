
use crate::error::{blst_err_to_mithril, MultiSignatureError};
use blst::min_sig::{
    AggregatePublicKey, AggregateSignature, PublicKey as BlstVk, SecretKey as BlstSk,
    Signature as BlstSig,
};
use blst::{blst_p1, blst_p2, p1_affines, p2_affines, BLST_ERROR};
use crate::bls_multi_sig::pop::ProofOfPossession;
use crate::bls_multi_sig::signing_key::SigningKey;
use crate::bls_multi_sig::verification_key::VerificationKey;

/// MultiSig public key, contains the verification key and the proof of possession.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct VerificationKeyPoP {
    /// The verification key.
    pub vk: VerificationKey,
    /// Proof of Possession.
    pub pop: ProofOfPossession,
}

impl VerificationKeyPoP {
    /// if `e(k1,g2) = e(H_G1("PoP" || mvk),mvk)` and `e(g1,mvk) = e(k2,g2)`
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // If we are really looking for performance improvements, we can combine the
    // two final exponentiations (for verifying k1 and k2) into a single one.
    pub fn check(&self) -> Result<(), MultiSignatureError> {
        match self.vk.0.validate() {
            Ok(_) => {
                let result = verify_pairing(&self.vk, &self.pop);
                if !(self.pop.k1.verify(false, POP, &[], &[], &self.vk.0, false)
                    == BLST_ERROR::BLST_SUCCESS
                    && result)
                {
                    return Err(MultiSignatureError::KeyInvalid(Box::new(*self)));
                }
                Ok(())
            }
            Err(e) => blst_err_to_mithril(e, None, Some(self.vk)),
        }
    }

    /// Convert to a 144 byte string.
    ///
    /// # Layout
    /// The layout of a `PublicKeyPoP` encoding is
    /// * Public key
    /// * Proof of Possession
    pub fn to_bytes(self) -> [u8; 192] {
        let mut vkpop_bytes = [0u8; 192];
        vkpop_bytes[..96].copy_from_slice(&self.vk.to_bytes());
        vkpop_bytes[96..].copy_from_slice(&self.pop.to_bytes());
        vkpop_bytes
    }

    /// Deserialize a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mvk = VerificationKey::from_bytes(&bytes[..96])?;

        let pop = ProofOfPossession::from_bytes(&bytes[96..])?;

        Ok(Self { vk: mvk, pop })
    }
}

impl From<&SigningKey> for VerificationKeyPoP {
    /// Convert a secret key into a `VerificationKeyPoP` by simply converting to a
    /// `MspMvk` and `MspPoP`.
    fn from(sk: &SigningKey) -> Self {
        Self {
            vk: sk.into(),
            pop: sk.into(),
        }
    }
}
