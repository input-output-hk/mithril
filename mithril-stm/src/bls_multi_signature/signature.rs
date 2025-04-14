use crate::bls_multi_signature::helper::unsafe_helpers::{
    p1_affine_to_sig, p2_affine_to_vk, sig_to_p1, vk_from_p2_affine,
};
use crate::bls_multi_signature::verification_key::VerificationKey;
use crate::error::{blst_err_to_mithril, MultiSignatureError};
use crate::stm_legacy::Index;
use blake2::Blake2b;
use blake2::{Blake2b512, Digest};
use blst::min_sig::{AggregateSignature, PublicKey as BlstVk, Signature as BlstSig};
use blst::{blst_p1, blst_p2, p1_affines, p2_affines};
use digest::consts::U16;
use std::cmp::Ordering;
use std::iter::Sum;

/// MultiSig signature, which is a wrapper over the `BlstSig` type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature(pub BlstSig);

impl Signature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: &[u8], mvk: &VerificationKey) -> Result<(), MultiSignatureError> {
        blst_err_to_mithril(
            self.0.validate(true).map_or_else(
                |e| e,
                |_| {
                    self.0
                        .verify(false, msg, &[], &[], &mvk.to_blst_vk(), false)
                },
            ),
            Some(*self),
            None,
        )
    }

    /// Dense mapping function indexed by the index to be evaluated.
    /// We hash the signature to produce a 64 bytes integer.
    /// The return value of this function refers to
    /// `ev = H("map" || msg || index || σ) <- MSP.Eval(msg,index,σ)` given in paper.
    pub fn eval(&self, msg: &[u8], index: Index) -> [u8; 64] {
        let hasher = Blake2b512::new()
            .chain_update(b"map")
            .chain_update(msg)
            .chain_update(index.to_le_bytes())
            .chain_update(self.to_bytes())
            .finalize();

        let mut output = [0u8; 64];
        output.copy_from_slice(hasher.as_slice());

        output
    }

    /// Convert an `Signature` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 48] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `MspSig`.
    ///
    /// # Error
    /// Returns an error if the byte string does not represent a point in the curve.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstSig::sig_validate(&bytes[..48], true) {
            Ok(sig) => Ok(Self(sig)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
    }

    /// Compare two signatures. Used for PartialOrd impl, used to rank signatures. The comparison
    /// function can be anything, as long as it is consistent across different nodes.
    fn cmp_msp_sig(&self, other: &Self) -> Ordering {
        let self_bytes = self.to_bytes();
        let other_bytes = other.to_bytes();
        let mut result = Ordering::Equal;

        for (i, j) in self_bytes.iter().zip(other_bytes.iter()) {
            result = i.cmp(j);
            if result != Ordering::Equal {
                return result;
            }
        }
        result
    }

    /// Aggregate a slice of verification keys and Signatures by first hashing the
    /// signatures into random scalars, and multiplying the signature and verification
    /// key with the resulting value. This follows the steps defined in Figure 6,
    /// `Aggregate` step.
    pub fn aggregate(
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(VerificationKey, Signature), MultiSignatureError> {
        if vks.len() != sigs.len() || vks.is_empty() {
            return Err(MultiSignatureError::AggregateSignatureInvalid);
        }

        if vks.len() < 2 {
            return Ok((vks[0], sigs[0]));
        }

        let mut hashed_sigs = Blake2b::<U16>::new();
        for sig in sigs {
            hashed_sigs.update(sig.to_bytes());
        }

        // First we generate the scalars
        let mut scalars = Vec::with_capacity(vks.len() * 128);
        let mut signatures = Vec::with_capacity(vks.len());
        for (index, sig) in sigs.iter().enumerate() {
            let mut hasher = hashed_sigs.clone();
            hasher.update(index.to_be_bytes());
            signatures.push(sig.0);
            scalars.extend_from_slice(hasher.finalize().as_slice());
        }

        let transmuted_vks: Vec<blst_p2> = vks.iter().map(vk_from_p2_affine).collect();
        let transmuted_sigs: Vec<blst_p1> = signatures.iter().map(sig_to_p1).collect();

        let grouped_vks = p2_affines::from(transmuted_vks.as_slice());
        let grouped_sigs = p1_affines::from(transmuted_sigs.as_slice());

        let aggr_vk: BlstVk = p2_affine_to_vk(&grouped_vks.mult(&scalars, 128));
        let aggr_sig: BlstSig = p1_affine_to_sig(&grouped_sigs.mult(&scalars, 128));

        Ok((VerificationKey(aggr_vk), Signature(aggr_sig)))
    }

    /// Verify a set of signatures with their corresponding verification keys using the
    /// aggregation mechanism of Figure 6.
    pub fn verify_aggregate(
        msg: &[u8],
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(), MultiSignatureError> {
        let (aggr_vk, aggr_sig) = Self::aggregate(vks, sigs)?;

        blst_err_to_mithril(
            aggr_sig
                .0
                .verify(false, msg, &[], &[], &aggr_vk.to_blst_vk(), false),
            Some(aggr_sig),
            None,
        )
    }

    /// Batch verify several sets of signatures with their corresponding verification keys.
    #[cfg(feature = "batch-verify-aggregates")]
    pub fn batch_verify_aggregates(
        msgs: &[Vec<u8>],
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(), MultiSignatureError> {
        let batched_sig: BlstSig = match AggregateSignature::aggregate(
            &(sigs.iter().map(|sig| &sig.0).collect::<Vec<&BlstSig>>()),
            false,
        ) {
            Ok(sig) => BlstSig::from_aggregate(&sig),
            Err(e) => return blst_err_to_mithril(e, None, None),
        };

        let p2_vks: Vec<BlstVk> = vks.iter().map(|vk| vk.to_blst_vk()).collect();
        let p2_vks_ref: Vec<&BlstVk> = p2_vks.iter().collect();
        let slice_msgs = msgs
            .iter()
            .map(|msg| msg.as_slice())
            .collect::<Vec<&[u8]>>();

        blst_err_to_mithril(
            batched_sig.aggregate_verify(false, &slice_msgs, &[], &p2_vks_ref, false),
            None,
            None,
        )
        .map_err(|_| MultiSignatureError::BatchInvalid)
    }
}

impl<'a> Sum<&'a Self> for Signature {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let signatures: Vec<&BlstSig> = iter.map(|x| &x.0).collect();
        assert!(!signatures.is_empty(), "One cannot add an empty vector");
        let aggregate = AggregateSignature::aggregate(&signatures, false)
            .expect("An MspSig is always a valid signature. This function only fails if signatures is empty or if the signatures are invalid, none of which can happen.")
            .to_signature();

        Self(aggregate)
    }
}

impl PartialOrd for Signature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for Signature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_sig(other)
    }
}
