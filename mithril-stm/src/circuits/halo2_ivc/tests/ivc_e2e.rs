use crate::{
    BlstrsEmulation, CircuitCurve, NativeChip, NativeGadget, P2RDecompositionChip, SelfEmulation,
};

type S = BlstrsEmulation;
type F = <S as SelfEmulation>::F;
type C = <S as SelfEmulation>::C;

type E = <S as SelfEmulation>::Engine;
type CBase = <C as CircuitCurve>::Base;

type NG = NativeGadget<F, P2RDecompositionChip<F>, NativeChip<F>>;

#[cfg(feature = "truncated-challenges")]
const K: u32 = 19;

#[cfg(not(feature = "truncated-challenges"))]
const K: u32 = 19;

pub const PREIMAGE_SIZE: usize = 190;

pub mod circuit;
pub mod config;
pub mod gadget;
pub mod io;
pub mod state;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Accumulator, AssignedAccumulator, Bls12, CERT_VK_NAME, CircuitTranscript, IVC_ONE_NAME,
        Instantiable, KZGCommitmentScheme, ParamsKZG, PoseidonState, Relation, Transcript,
        certificate::Certificate,
        circuit_model, create_proof,
        ivc_one::{
            circuit::IvcCircuit,
            state::{Global, State, Witness, fixed_bases_and_names, trivial_acc},
        },
        keygen_pk, keygen_vk_with_k,
        merkle_tree::{MTLeaf, MerklePath, MerkleTree},
        prepare,
        protocol_message::{AggregateVerificationKey, ProtocolMessage, ProtocolMessagePartKey},
        schnorr_signature::{
            SigningKey as SchnorrSigningKey, VerificationKey as SchnorrVerificationKey,
        },
        unique_signature::{Signature, SigningKey, VerificationKey},
        utils::jubjub_base_from_le_bytes,
    };
    use ff::Field;
    use group::Group;
    use midnight_proofs::utils::SerdeFormat;
    use midnight_zk_stdlib as zk_lib;
    use midnight_zk_stdlib::utils::plonk_api::filecoin_srs;
    use rand_core::OsRng;
    use std::collections::BTreeMap;
    use std::fs::File;
    use std::io::{BufReader, Cursor, Write};
    use std::time::Instant;

    const NUM_CERT: usize = 3;
    fn open(k: u32) -> ParamsKZG<Bls12> {
        let path = format!("examples/assets/params_kzg_unsafe_{}", k);
        let file = File::open(path).unwrap();
        let mut reader = BufReader::new(file);
        let params: ParamsKZG<Bls12> =
            ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytesUnchecked).unwrap();

        params
    }

    fn create_merkle_tree(n: usize) -> (Vec<SigningKey>, Vec<MTLeaf>, MerkleTree) {
        //  let mut rng = ChaCha20Rng::seed_from_u64(1234);
        let mut rng = OsRng;

        let mut sks = Vec::new();
        let mut leaves = Vec::new();
        for _ in 0..n {
            let sk = SigningKey::generate(&mut rng);
            let vk = VerificationKey::from(&sk); // Replace this with actual initialization if provided
            leaves.push(MTLeaf(vk, -F::ONE));
            sks.push(sk);
        }
        let tree = MerkleTree::create(&leaves);

        (sks, leaves, tree)
    }

    fn setup(
        quorum: u32,
    ) -> (
        SchnorrVerificationKey,
        F,
        Certificate,
        Vec<Vec<(MTLeaf, MerklePath, Signature, u32)>>,
        Vec<Vec<F>>,
        Vec<State>,
        Vec<Witness>,
    ) {
        //  let mut rng = ChaCha20Rng::seed_from_u64(1234);
        let mut rng = OsRng;
        let num_signers: usize = 3000;
        let depth = num_signers.next_power_of_two().trailing_zeros();
        let num_lotteries = quorum * 10;
        let total_stake = 1000000u64;

        let relation = Certificate::new(quorum, num_lotteries, depth);
        println!("Circuit {:?}", relation);

        let (sks, leaves, merkle_tree) = create_merkle_tree(num_signers);
        let avk =
            AggregateVerificationKey::new(merkle_tree.to_merkle_tree_commitment(), total_stake);

        // Genesis certificate
        let genesis_sk = SchnorrSigningKey::generate(&mut rng);
        let genesis_vk = SchnorrVerificationKey::from(&genesis_sk);
        // Genesis epoch number may be zero
        let genesis_epoch = 5u64;
        let genesis_next_merkle_root = merkle_tree.root();
        let genesis_next_protocol_params = F::from(7u64);

        let (genesis_msg, genesis_preimage) = {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(ProtocolMessagePartKey::Digest, vec![2u8; 32]);
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                avk.clone().into(),
            );
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextProtocolParameters,
                genesis_next_protocol_params.to_bytes_le().to_vec(),
            );
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CurrentEpoch,
                genesis_epoch.to_le_bytes().into(),
            );
            let preimage = protocol_message.get_preimage();
            let msg = protocol_message.compute_hash();
            (jubjub_base_from_le_bytes(&msg), preimage)
        };
        // Sign the genesis certificate
        let genesis_sig = genesis_sk.sign(&[genesis_msg], &mut rng);
        genesis_sig.verify(&[genesis_msg], &genesis_vk).unwrap();

        let mut current_epoch = genesis_epoch;
        let mut counter = F::ONE;
        let mut ivc_next_states = vec![State::new(
            counter,
            genesis_msg,
            F::ZERO,
            genesis_next_merkle_root,
            F::ZERO,
            genesis_next_protocol_params,
            F::from(current_epoch),
        )];
        let mut ivc_witnesses = vec![Witness::new(
            genesis_sig.clone(),
            F::ZERO,
            F::ZERO,
            genesis_preimage.try_into().unwrap(),
        )];

        let mut cert_witnesses = vec![vec![]];
        let mut cert_instances = vec![vec![]];

        for i in 1..NUM_CERT {
            counter += F::ONE;
            current_epoch += 1;

            // In the test, we only consider a simple case where each certificate has:
            // merkle_root = next_merkle_root = genesis_next_merkle_root
            // protocol_params = next_protocol_params = genesis_next_protocol_params
            let (msg, preimage) = {
                let mut protocol_message = ProtocolMessage::new();
                protocol_message.set_message_part(ProtocolMessagePartKey::Digest, vec![3u8; 32]);
                protocol_message.set_message_part(
                    ProtocolMessagePartKey::NextAggregateVerificationKey,
                    avk.clone().into(),
                );
                protocol_message.set_message_part(
                    ProtocolMessagePartKey::NextProtocolParameters,
                    genesis_next_protocol_params.to_bytes_le().to_vec(),
                );
                protocol_message.set_message_part(
                    ProtocolMessagePartKey::CurrentEpoch,
                    current_epoch.to_le_bytes().into(),
                );
                let preimage = protocol_message.get_preimage();
                let msg = protocol_message.compute_hash();
                (jubjub_base_from_le_bytes(&msg), preimage)
            };

            ivc_next_states.push(State::new(
                counter,
                msg,
                genesis_next_merkle_root,
                genesis_next_merkle_root,
                genesis_next_protocol_params,
                genesis_next_protocol_params,
                F::from(current_epoch),
            ));

            ivc_witnesses.push(Witness::new(
                genesis_sig.clone(),
                genesis_next_merkle_root,
                msg,
                preimage.try_into().unwrap(),
            ));

            let mut cert_witness = vec![];
            for j in 0..quorum as usize {
                let usk = sks[j].clone();
                let uvk = leaves[j].0;
                let sig = usk.sign(&[genesis_next_merkle_root, msg], &mut OsRng);
                sig.verify(&[genesis_next_merkle_root, msg], &uvk).unwrap();

                let merkle_path = merkle_tree.get_path(j);
                let computed_root = merkle_path.compute_root(leaves[j]);
                assert_eq!(genesis_next_merkle_root, computed_root);

                // Any index is eligible as target is set to be the maximum
                cert_witness.push((leaves[j], merkle_path, sig, (j + 1) as u32));
            }

            let instance = Certificate::format_instance(&(genesis_next_merkle_root, msg)).unwrap();

            cert_witnesses.push(cert_witness);
            cert_instances.push(instance);
        }

        (
            genesis_vk,
            genesis_msg,
            relation,
            cert_witnesses,
            cert_instances,
            ivc_next_states,
            ivc_witnesses,
        )
    }

    macro_rules! prove_ivc {
        ($TranscriptType:ty, $circuit:expr, $srs:expr, $pk:expr, $public_inputs:expr) => {{
            // Initialize the transcript
            let mut transcript = CircuitTranscript::<$TranscriptType>::init();
            // Call create_proof
            let res = create_proof::<
                F,
                KZGCommitmentScheme<E>,
                CircuitTranscript<$TranscriptType>,
                IvcCircuit,
            >(
                $srs,
                $pk,
                &[$circuit.clone()],
                1,
                &[&[&[], $public_inputs]],
                OsRng,
                &mut transcript,
            );

            // Handle error
            res.unwrap_or_else(|e| panic!("create proof error {:?}", e));

            // Finalize transcript and return proof bytes
            transcript.finalize()
        }};
    }

    macro_rules! verify_prepare {
        ($TranscriptType:ty, $proof:expr, $vk:expr, $public_inputs:expr) => {{
            // Start a transcript from proof bytes
            let mut transcript = CircuitTranscript::<$TranscriptType>::init_from_bytes($proof);
            // Run prepare
            let dual_msm =
                prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<$TranscriptType>>(
                    $vk,
                    &[&[C::identity()]],
                    &[&[$public_inputs]],
                    &mut transcript,
                )
                .expect("Verification failed");
            transcript
                .assert_empty()
                .expect("Transcript should be empty");
            dual_msm
        }};
    }

    #[test]
    fn test_ivc_one() {
        let quorum = 3;
        const K_INNER: u32 = 13;

        let k_max = K.max(K_INNER);
        //let srs = filecoin_srs(K);
        let srs = open(k_max);
        let srs_verifier = srs.verifier_params();

        let ivc_srs = {
            let mut ivc_srs = srs.clone();
            if K < k_max {
                ivc_srs.downsize(K)
            }
            ivc_srs
        };

        let cert_srs = {
            let mut cert_srs = srs.clone();
            if K_INNER < k_max {
                cert_srs.downsize(K_INNER)
            }
            cert_srs
        };

        // Create genesis certificate and normal certificates
        let (
            genesis_vk,
            genesis_msg,
            cert_relation,
            cert_witnesses,
            cert_instances,
            ivc_next_states,
            ivc_witnesses,
        ) = setup(quorum);

        // Set up cert circuit
        println!("setting up cert circuit...");
        let start = Instant::now();
        let cert_vk = zk_lib::setup_vk(&cert_srs, &cert_relation);
        let cert_pk = zk_lib::setup_pk(&cert_relation, &cert_vk);
        let duration = start.elapsed(); // Measure the elapsed time after proof generation.
        println!("cert circuit vk pk generation took: {:?}", duration);
        println!(
            "cert_vk hash {:?}, k_inner {:?}",
            cert_vk.vk().transcript_repr(),
            K_INNER
        );

        // Get fixed bases and their names from cert_vk
        let (cert_fixed_bases, cert_fixed_base_names) =
            fixed_bases_and_names(CERT_VK_NAME, cert_vk.vk());
        // Get trivial accumulator for certificate proof
        let cert_trivial_acc = trivial_acc(&cert_fixed_base_names);
        let mut cert_proofs = vec![vec![]];
        // We don't need to use cert_trivial_acc
        let mut cert_accs = vec![cert_trivial_acc];
        for i in 1..NUM_CERT {
            let start = Instant::now();
            let cert_proof = zk_lib::prove::<Certificate, PoseidonState<F>>(
                &cert_srs,
                &cert_pk,
                &cert_relation,
                &(cert_instances[i][0], cert_instances[i][1]),
                cert_witnesses[i].clone(),
                OsRng,
            )
            .expect("Proof generation should not fail");
            let duration = start.elapsed(); // Measure the elapsed time after proof generation.
            println!("Inner circuit proof generation took: {:?}", duration);

            // Verify the certificate snark proof to obtain accumulator cert_acc
            let cert_dual_msm = verify_prepare!(
                PoseidonState<F>,
                &cert_proof,
                cert_vk.vk(),
                &cert_instances[i]
            );
            assert!(cert_dual_msm.clone().check(&srs_verifier));
            // Convert dual_msm into accumulator
            let mut cert_acc: Accumulator<S> = cert_dual_msm.into();
            cert_acc.extract_fixed_bases(&cert_fixed_bases);
            assert!(cert_acc.check(&srs_verifier.s_g2().into(), &cert_fixed_bases));
            cert_acc.collapse();

            cert_proofs.push(cert_proof);
            cert_accs.push(cert_acc);
        }

        // Default ivc circuit for setting up proving key and verifying key
        let default_ivc_circuit = IvcCircuit::unknown(&cert_vk.vk());
        {
            let circuit_model = circuit_model::<_, 48, 32>(&default_ivc_circuit);
            println!("\n{:?}", circuit_model);
        }

        // Setting up ivc verifying key and proving key
        let start = Instant::now();
        let self_vk = keygen_vk_with_k(&ivc_srs, &default_ivc_circuit, K).unwrap();
        let self_pk = keygen_pk(self_vk.clone(), &default_ivc_circuit).unwrap();
        println!("Computed IVC circuit vk and pk in {:?} s", start.elapsed());

        {
            let mut buffer = Cursor::new(Vec::new());
            // Serialize the MidnightVK instance to the buffer in the RawBytes format
            self_vk.write(&mut buffer, SerdeFormat::RawBytes).unwrap();
            // Get the size of the serialized MidnightVK
            println!("ivc_vk length {:?}", buffer.get_ref().len());
            println!("ivc_vk hash {:?}", self_vk.transcript_repr());
        }

        // Get fixed bases and their names from ivc_vk
        let (self_fixed_bases, self_fixed_base_names) =
            fixed_bases_and_names(IVC_ONE_NAME, &self_vk);
        println!(
            "IVC fixed base name length {:?}",
            self_fixed_base_names.len()
        );

        // Combine fixed bases and their names for cert_vk and ivc_vk
        let mut combined_fixed_bases = BTreeMap::new();
        combined_fixed_bases.extend(cert_fixed_bases.clone());
        combined_fixed_bases.extend(self_fixed_bases.clone());
        let combined_fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();
        println!(
            "Combined fixed base name length {:?}",
            combined_fixed_base_names.len()
        );

        let global = Global::new(genesis_msg, genesis_vk, cert_vk.vk(), &self_vk);
        let mut state = State::genesis();
        let mut self_proof: Vec<u8> = vec![];
        // Trivial accumulator for the combined fixed bases
        let mut acc = trivial_acc(&combined_fixed_base_names);
        let mut next_acc = acc.clone();
        for i in 0..NUM_CERT {
            let circuit = IvcCircuit::new(
                global.clone(),
                state.clone(),
                ivc_witnesses[i].clone(),
                cert_proofs[i].clone(),
                self_proof.clone(),
                acc.clone(),
                &cert_vk.vk(),
                &self_vk,
            );

            // Set public inputs [global, next_state, next_acc]
            let public_inputs = [
                global.as_public_input(),
                ivc_next_states[i].as_public_input(),
                AssignedAccumulator::as_public_input(&next_acc),
            ]
            .concat();
            println!("instance length {:?}", public_inputs.len());

            let start = Instant::now();
            let proof = {
                if i < NUM_CERT - 1 {
                    prove_ivc!(
                        PoseidonState<F>,
                        circuit,
                        &ivc_srs,
                        &self_pk,
                        &public_inputs
                    )
                } else {
                    // Generate the last proof using blake2 for transcript hash
                    prove_ivc!(
                        blake2b_simd::State,
                        circuit,
                        &ivc_srs,
                        &self_pk,
                        &public_inputs
                    )
                }
            };
            println!("\n{i}-th IVC proof created in {:?}", start.elapsed());
            println!("IVC proof size {:?}", proof.len());

            let proof_acc: Accumulator<S> = {
                let start = Instant::now();
                let dual_msm = if i < NUM_CERT - 1 {
                    verify_prepare!(PoseidonState<F>, &proof, &self_vk, &public_inputs)
                } else {
                    verify_prepare!(blake2b_simd::State, &proof, &self_vk, &public_inputs)
                };
                assert!(dual_msm.clone().check(&srs_verifier));
                let duration = start.elapsed(); // Measure the elapsed time after proof generation.
                println!("IVC proof verification took: {:?}", duration);

                // Convert dual_msm into an accumulator
                let mut proof_acc: Accumulator<S> = dual_msm.into();
                proof_acc.extract_fixed_bases(&self_fixed_bases);
                proof_acc.collapse();
                proof_acc
            };

            // Update state and accumulator
            state = ivc_next_states[i].clone();
            acc = next_acc.clone();
            self_proof = proof;

            if i < NUM_CERT - 1 {
                // Prepare the next accumulator by combing next_acc with cert_acc and ivc proof_acc
                let mut accumulated = Accumulator::accumulate(&[
                    next_acc.clone(),
                    cert_accs[i + 1].clone(),
                    proof_acc,
                ]);
                accumulated.collapse();

                assert!(
                    accumulated.check(&srs_verifier.s_g2().into(), &combined_fixed_bases),
                    "IVC acc verification failed"
                );

                next_acc = accumulated;
            }
        }

        {
            // Print accumulator size
            use crate::ivc_one::io::Write;
            let mut buffer = Cursor::new(Vec::new());
            acc.write(&mut buffer, SerdeFormat::RawBytes).unwrap();
            println!("\naccumulator size {:?}", buffer.get_ref().len());
        }

        {
            // Benchmark verifying ivc proof and accumulator together
            let start = Instant::now();
            let total = 100;
            for _ in 0..total {
                let public_inputs = [
                    global.as_public_input(),
                    state.as_public_input(),
                    AssignedAccumulator::as_public_input(&acc),
                ]
                .concat();

                // todo: combine the msm on fixed bases and pair checking for dual_msm and acc without using poseidon
                // the current separate verification is faster
                let dual_msm =
                    verify_prepare!(blake2b_simd::State, &self_proof, &self_vk, &public_inputs);
                assert!(dual_msm.clone().check(&srs_verifier));
                assert!(
                    acc.check(&srs_verifier.s_g2().into(), &combined_fixed_bases),
                    "IVC acc verification failed"
                );
            }
            let duration = start.elapsed(); // Measure the elapsed time for proof verification.
            println!(
                "\nIVC proof and accumulator verification took: {:?}",
                duration / total
            );
        }
    }
}
