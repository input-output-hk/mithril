use crate::{
    BaseFieldElement, MembershipDigest, SingleSignature, StmResult,
    proof_system::{
        AggregateVerificationKeyForSnark, SnarkClerk,
        halo2_snark::{build_snark_message, compute_winning_lottery_indices},
    },
};

use super::WitnessEntry;

/// Public instance for the SNARK circuit: `(merkle_tree_root, message)`.
type Instance = (BaseFieldElement, BaseFieldElement);

/// SNARK prover input consisting of the public instance and a list of witness entries.
///
/// The instance holds the Merkle tree root and message (public inputs to the circuit).
/// The witness holds one [`WitnessEntry`] per winning lottery index, each containing
/// the signature, Merkle leaf, and Merkle path needed by the circuit.
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
    /// Prepare the SNARK prover input from single signatures.
    pub fn prepare_prover_input<D: MembershipDigest>(
        clerk: &SnarkClerk,
        signatures: &[SingleSignature],
        message: &[u8],
    ) -> StmResult<SnarkProverInput> {
        let avk: AggregateVerificationKeyForSnark<D> =
            clerk.compute_aggregate_verification_key_for_snark();
        let message_to_sign =
            build_snark_message(&avk.get_merkle_tree_commitment().root, &message)?;

        let valid_signatures_with_indices: Vec<SingleSignature> = signatures
            .iter()
            .filter_map(|sig| {
                let snark_sig = sig.snark_signature.as_ref()?;
                let reg_entry = clerk
                    .closed_key_registration
                    .get_registration_entry_for_index(&sig.signer_index)
                    .ok()?;
                let vk = reg_entry.get_verification_key_for_snark()?;
                snark_sig.verify(&vk, message, &avk).ok()?;
                let target_value = reg_entry.get_lottery_target_value()?;
                let indices = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    &message_to_sign,
                    &snark_sig.get_schnorr_signature(),
                    target_value,
                )
                .ok()?;
                let mut new_sig = sig.clone();
                new_sig.set_snark_signature_indices(&indices);
                Some(new_sig)
            })
            .collect();

        let _unique_index_signature_map = SnarkClerk::select_valid_signatures_for_k_indices(
            &clerk.parameters,
            &valid_signatures_with_indices,
        )?;

        let instance = (message_to_sign[0], message_to_sign[1]);

        Ok(SnarkProverInput {
            witness: Vec::<WitnessEntry>::new(),
            instance,
        })
    }

    /// Return the public instance `(merkle_tree_root, message)`.
    pub fn get_instance(&self) -> Instance {
        self.instance
    }

    /// Return a reference to the witness entries.
    pub fn get_witness(&self) -> &[WitnessEntry] {
        &self.witness
    }
}
