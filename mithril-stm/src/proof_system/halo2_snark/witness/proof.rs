use crate::{
    MembershipDigest, SingleSignature, StmResult,
    proof_system::{
        AggregateVerificationKeyForSnark, SnarkClerk,
        halo2_snark::{build_snark_message, compute_winning_lottery_indices},
    },
};

use super::{Instance, SignatureRegistrationEntry, SignerWitness};

#[allow(dead_code)]
pub struct SnarkProof<D: MembershipDigest> {
    instance: Instance,
    witness: Vec<SignerWitness<D>>,
}

#[allow(dead_code)]
impl<D: MembershipDigest> SnarkProof<D> {
    pub fn aggregate_signatures(
        clerk: &SnarkClerk,
        signatures: &[SingleSignature],
        message: &[u8],
    ) -> StmResult<SnarkProof<D>> {
        let avk: AggregateVerificationKeyForSnark<D> =
            clerk.compute_aggregate_verification_key_for_snark();
        let message_to_sign = build_snark_message(&avk.get_merkle_tree_commitment().root, message)?;

        let mut sig_reg_list: Vec<SignatureRegistrationEntry> = signatures
            .iter()
            .filter_map(|sig| {
                let snark_sig = sig.snark_signature.clone()?;
                let reg_entry =
                    clerk.get_snark_registration_entry(sig.signer_index).ok().flatten()?;
                Some(SignatureRegistrationEntry::new(snark_sig, reg_entry))
            })
            .collect();

        sig_reg_list.retain_mut(|entry| {
            let reg = entry.get_registration_entry();
            if entry.get_signature().verify(&reg.0, message, &avk).is_ok()
                && let Ok(indices) = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    &message_to_sign,
                    &entry.get_signature().get_schnorr_signature(),
                    reg.1,
                )
            {
                entry.set_indices(&indices);
                return true;
            }
            false
        });

        let _deduped_signatures =
            SnarkClerk::select_valid_signatures_for_k_indices(&clerk.parameters, &sig_reg_list)?;

        // TODO: build Instance and SignerWitness entries from deduped_signatures
        Ok(SnarkProof {
            instance: Instance::new(message_to_sign[0], message_to_sign[1]),
            witness: Vec::new(),
        })
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Initializer, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
        Signer, SingleSignature, proof_system::SnarkClerk,
    };

    use super::*;

    type D = MithrilMembershipDigest;

    #[test]
    fn deduplicate_indices() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let parameters = Parameters {
            m: 100,
            k: 20,
            phi_f: 0.50,
        };

        let message = [0u8; 32];

        let mut initializers = Vec::new();
        let mut key_reg = KeyRegistration::initialize();

        let stakes = [10, 8, 13, 10];

        for &stake in &stakes {
            let init = Initializer::new(parameters, stake, &mut rng);
            initializers.push(init.clone());
            let entry = RegistrationEntry::try_from(init).unwrap();
            key_reg.register_by_entry(&entry).unwrap();
        }

        let closed_key_reg = key_reg.close_registration();

        let mut signatures: Vec<SingleSignature> = Vec::new();
        for init in initializers {
            let signer: Signer<D> = init.clone().try_create_signer(&closed_key_reg).unwrap();
            let signature = signer.create_single_signature(&message).unwrap();
            signatures.push(signature);
        }

        let clerk =
            SnarkClerk::new_clerk_from_closed_key_registration(&parameters, &closed_key_reg);

        let _proof: SnarkProof<D> =
            SnarkProof::aggregate_signatures(&clerk, &signatures, &message).unwrap();
    }
}
