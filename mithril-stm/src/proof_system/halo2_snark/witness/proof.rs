use crate::{
    MembershipDigest, RegisterError, SingleSignature, StmResult,
    proof_system::{
        AggregateVerificationKeyForSnark, SnarkClerk,
        halo2_snark::{build_snark_message, compute_winning_lottery_indices},
    },
};

use super::{Instance, SignerWitness};

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

        // // Print the signatures (bls sig and snark sig) before verification
        // println!("Input signatures: {} total", signatures.len());
        // for (i, sig) in signatures.iter().enumerate() {
        //     println!(
        //         "  [{}] signer_index: {}, has_snark_sig: {}",
        //         i,
        //         sig.signer_index,
        //         sig.snark_signature.is_some()
        //     );
        // }

        // Collect the snark signatures and their registration entries by filtering
        // the snark signatures and mapping them to their corresponding registration entries
        let mut snark_sig_reg_list: Vec<_> = signatures
            .iter()
            .filter_map(|sig| {
                sig.snark_signature
                    .clone()
                    .map(|snark_sig| (sig.signer_index, snark_sig))
            })
            .map(|(signer_index, snark_sig)| {
                let reg_entry = clerk
                    .get_snark_registration_entry(signer_index)?
                    .ok_or(RegisterError::MissingSnarkRegistrationEntry(signer_index))?;
                Ok((snark_sig, reg_entry))
            })
            .collect::<StmResult<_>>()?;

        // println!("After collect: {} entries", snark_sig_reg_list.len());

        // Verify each SNARK signature against its registration entry.
        // If valid, compute the winning lottery indices and set them in the signature.
        // Retain only the valid signatures and their corresponding registration entries in the list.
        snark_sig_reg_list.retain_mut(|(snark_sig, reg_entry)| {
            if snark_sig.verify(&reg_entry.0, message, &avk).is_ok() {
                if let Ok(indices) = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    &message_to_sign,
                    &snark_sig.get_schnorr_signature(),
                    reg_entry.1,
                ) {
                    snark_sig.set_indices(&indices);
                    return true;
                }
            }
            false
        });

        // // Print verified signatures with computed indices
        // println!("After retain_mut: {} entries", snark_sig_reg_list.len());
        // for (i, (sig, reg)) in snark_sig_reg_list.iter().enumerate() {
        //     println!("  [{}] indices: {:?}", i, sig.get_indices(),);
        // }

        // TODO: build Instance and SignerWitness entries from snark_sig_reg_list
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
        BlsVerificationKeyProofOfPossession, Initializer, KeyRegistration, MithrilMembershipDigest,
        Parameters, RegistrationEntry, Signer, SingleSignature,
        proof_system::{ConcatenationProofSigner, SnarkClerk},
        signature_scheme::BlsSigningKey,
    };

    use super::SnarkProof;

    type D = MithrilMembershipDigest;

    #[test]
    fn aggregate_signatures_with_mixed_entries() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let params = Parameters {
            m: 100,
            k: 1,
            phi_f: 0.2,
        };

        let message = [0u8; 32];

        let mut initializers = Vec::new();
        let mut entries = Vec::new();

        let mut key_reg = KeyRegistration::initialize();

        let stakes = [1, 5, 10, 20];

        // 4 full initializers
        for &stake in &stakes {
            let init = Initializer::new(params, stake, &mut rng);
            initializers.push(init.clone());
            let entry = RegistrationEntry::try_from(init).unwrap();
            entries.push(entry);
        }

        // Initializer without snark
        let sk = BlsSigningKey::generate(&mut rng);
        let vk_pop = BlsVerificationKeyProofOfPossession::from(&sk);
        let init = Initializer {
            stake: 40,
            parameters: params,
            bls_signing_key: sk.clone(),
            bls_verification_key_proof_of_possession: vk_pop.clone(),
            #[cfg(feature = "future_snark")]
            schnorr_signing_key: None,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key: None,
        };
        initializers.push(init.clone());
        let entry = RegistrationEntry::try_from(init).unwrap();
        entries.push(entry);

        // Register all entries
        for entry in &entries {
            key_reg.register_by_entry(entry).unwrap();
        }

        // Close the registration
        let closed_key_reg = key_reg.close_registration();

        // Create signatures for the first 4 initializers (with snark) and the last initializer (without snark)
        let mut signatures: Vec<SingleSignature> = Vec::new();
        for i in 0..4 {
            let signer: Signer<D> =
                initializers[i].clone().try_create_signer(&closed_key_reg).unwrap();
            let signature = signer.create_single_signature(&message).unwrap();
            signatures.push(signature);
        }
        let signer: Signer<D> = Signer::new(
            4,
            ConcatenationProofSigner::new(
                40,
                closed_key_reg.total_stake,
                params,
                sk,
                vk_pop.vk,
                closed_key_reg.to_merkle_tree().to_merkle_tree_batch_commitment(),
            ),
            closed_key_reg.clone(),
            params,
            40,
            #[cfg(feature = "future_snark")]
            None,
        );
        let signature = signer.create_single_signature(&message).unwrap();
        signatures.push(signature);

        let clerk = SnarkClerk::new_clerk_from_closed_key_registration(&params, &closed_key_reg);
        let _snark_proof: SnarkProof<D> =
            SnarkProof::aggregate_signatures(&clerk, &signatures, &message).unwrap();
    }
}
