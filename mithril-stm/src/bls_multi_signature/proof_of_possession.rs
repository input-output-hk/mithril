use crate::bls_multi_signature::helper::unsafe_helpers::{
    compress_p1, scalar_to_pk_in_g1, uncompress_p1,
};
use crate::bls_multi_signature::signing_key::SigningKey;
use crate::bls_multi_signature::POP;
use crate::error::{blst_err_to_mithril, MultiSignatureError};
use blst::blst_p1;
use blst::min_sig::Signature as BlstSig;

/// MultiSig proof of possession, which contains two elements from G1. However,
/// the two elements have different types: `k1` is represented as a BlstSig
/// as it has the same structure, and this facilitates its verification. On
/// the other hand, `k2` is a G1 point, as it does not share structure with
/// the BLS signature, and we need to have an ad-hoc verification mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofOfPossession {
    k1: BlstSig,
    k2: blst_p1,
}

impl ProofOfPossession {
    /// Convert to a 96 byte string.
    ///
    /// # Layout
    /// The layout of a `MspPoP` encoding is
    /// * K1 (G1 point)
    /// * K2 (G1 point)
    pub fn to_bytes(self) -> [u8; 96] {
        let mut pop_bytes = [0u8; 96];
        pop_bytes[..48].copy_from_slice(&self.k1.to_bytes());

        pop_bytes[48..].copy_from_slice(&compress_p1(&self.k2)[..]);
        pop_bytes
    }

    /// Deserialize a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let k1 = match BlstSig::from_bytes(&bytes[..48]) {
            Ok(key) => key,
            Err(e) => {
                return Err(blst_err_to_mithril(e, None, None)
                    .expect_err("If it passed, blst returns and error different to SUCCESS."))
            }
        };

        let k2 = uncompress_p1(&bytes[48..96])?;

        Ok(Self { k1, k2 })
    }

    pub(crate) fn to_k1(self) -> BlstSig {
        self.k1
    }

    pub(crate) fn to_k2(self) -> blst_p1 {
        self.k2
    }
}

impl From<&SigningKey> for ProofOfPossession {
    /// Convert a secret key into an `MspPoP`. This is performed by computing
    /// `k1 =  H_G1(b"PoP" || mvk)` and `k2 = g1 * sk` where `H_G1` hashes into
    /// `G1` and `g1` is the generator in `G1`.
    fn from(sk: &SigningKey) -> Self {
        let k1 = sk.to_blst_sk().sign(POP, &[], &[]);
        let k2 = scalar_to_pk_in_g1(&sk.to_blst_sk());
        Self { k1, k2 }
    }
}
