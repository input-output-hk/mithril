use crate::{
    BaseFieldElement, MembershipDigest, SingleSignature, StmResult,
    proof_system::{
        AggregateVerificationKeyForSnark, SnarkClerk,
        halo2_snark::{build_snark_message, compute_winning_lottery_indices},
    },
};

use super::WitnessEntry;

/// Public instance for the SNARK circuit, consisting of the Merkle tree root and the message.
/// Both elements are base field scalars consumed directly by the halo2 circuit as public inputs.
type Instance = (BaseFieldElement, BaseFieldElement);

/// Prover input for the SNARK circuit, bundling public and private data.
/// The **instance** carries the Merkle tree root and the signed message, the two public inputs
/// exposed to the verifier. The **witness** contains one `WitnessEntry` per winning lottery index,
/// each providing the Schnorr signature, Merkle leaf, and authentication path that the providing
/// the Schnorr signature, Merkle leaf, and authentication path that the circuit checks privately.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct SnarkProverInput {
    /// Public inputs to the SNARK circuit.
    instance: Instance,
    /// Per-winning-lottery-index witness data.
    witness: Vec<WitnessEntry>,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SnarkProverInput {
    /// Build the SNARK prover input from a set of single signatures.
    ///
    /// This method orchestrates the full SNARK prover input preparation pipeline:
    /// 1. **Signature validation** each signature's Schnorr component is verified against the
    ///    signer's registered verification key and the aggregate verification key.
    /// 2. **Lottery evaluation** winning lottery indices are computed for every valid signature
    ///    and attached to a cloned copy of the signature.
    /// 3. **Deduplication** `SnarkClerk::select_valid_signatures_for_k_indices` keeps exactly `k`
    ///    unique lottery indices, breaking ties by smallest Schnorr signature.
    /// 4. **Witness construction** `WitnessEntry::create_witness` builds the per-index witness data
    ///    (Merkle leaf, authentication path, signature).
    ///
    /// Returns an error if fewer than `k` unique winning indices can be collected, or if a Merkle
    /// path conversion fails.
    pub fn prepare_prover_input<D: MembershipDigest>(
        clerk: &SnarkClerk,
        signatures: &[SingleSignature],
        message: &[u8],
    ) -> StmResult<SnarkProverInput> {
        let avk: AggregateVerificationKeyForSnark<D> =
            clerk.compute_aggregate_verification_key_for_snark();
        let message_to_sign = build_snark_message(&avk.get_merkle_tree_commitment().root, message)?;

        let valid_signatures_with_indices = Self::collect_valid_signatures_with_indices(
            clerk,
            signatures,
            message,
            &message_to_sign,
            &avk,
        );

        let unique_index_signature_map = SnarkClerk::select_valid_signatures_for_k_indices(
            &clerk.parameters,
            &valid_signatures_with_indices,
        )?;

        let witness = WitnessEntry::create_witness::<D>(unique_index_signature_map, clerk)?;

        let instance = (message_to_sign[0], message_to_sign[1]);

        Ok(SnarkProverInput { witness, instance })
    }

    /// Filter and validate signatures that have a SNARK component.
    ///
    /// For each signature, this method verifies the Schnorr component against the signer's winning
    /// lottery indices and attaches them to a cloned copy of the signature. Signatures that lack a
    /// SNARK component, have no registration entry, fail verification, or yield no winning indices
    /// are silently skipped.
    fn collect_valid_signatures_with_indices<D: MembershipDigest>(
        clerk: &SnarkClerk,
        signatures: &[SingleSignature],
        message: &[u8],
        message_to_sign: &[BaseFieldElement; 2],
        aggregate_verification_key: &AggregateVerificationKeyForSnark<D>,
    ) -> Vec<SingleSignature> {
        signatures
            .iter()
            .filter_map(|sig| {
                let snark_sig = sig.snark_signature.as_ref()?;
                let reg_entry =
                    clerk.get_snark_registration_entry(sig.signer_index).ok().flatten()?;
                snark_sig
                    .verify(&reg_entry.0, message, aggregate_verification_key)
                    .ok()?;
                let indices = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    message_to_sign,
                    &snark_sig.get_schnorr_signature(),
                    reg_entry.1,
                )
                .ok()?;
                let mut new_sig = sig.clone();
                new_sig.set_snark_signature_indices(&indices);
                Some(new_sig)
            })
            .collect()
    }

    /// Return the public instance as a `(merkle_tree_root, message)` pair.
    pub fn get_instance(&self) -> Instance {
        self.instance
    }

    /// Return a reference to the witness entries.
    pub fn get_witness(&self) -> &[WitnessEntry] {
        &self.witness
    }
}
