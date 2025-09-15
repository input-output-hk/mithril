mod aggregate_key;
mod basic_verifier;
mod clerk;
mod proof;
mod signature;

pub use aggregate_key::*;
pub use basic_verifier::*;
pub use clerk::*;
pub use proof::*;
pub use signature::*;

#[cfg(test)]
mod tests {
    use core::panic;
    use std::collections::{HashMap, HashSet};

    use blake2::{Blake2b, digest::consts::U32};
    use proptest::{
        collection::{hash_map, vec},
        prelude::*,
        test_runner::{RngAlgorithm::ChaCha, TestRng},
    };
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    use crate::bls_multi_signature::BlsVerificationKey;
    use crate::merkle_tree::MerkleBatchPath;
    use crate::{
        AggregateSignature, AggregationError, BasicVerifier, Clerk, CoreVerifierError, Initializer,
        KeyRegistration, Parameters, Signer, SingleSignature, SingleSignatureWithRegisteredParty,
        Stake,
    };

    type Sig = AggregateSignature<D>;
    type D = Blake2b<U32>;

    fn setup_equal_parties(params: Parameters, nparties: usize) -> Vec<Signer<D>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: Parameters, stake: Vec<Stake>) -> Vec<Signer<D>> {
        let mut kr = KeyRegistration::init();
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = ChaCha20Rng::from_seed(trng.random());

        #[allow(clippy::needless_collect)]
        let ps = stake
            .into_iter()
            .map(|stake| {
                let p = Initializer::new(params, stake, &mut rng);
                kr.register(stake, p.pk).unwrap();
                p
            })
            .collect::<Vec<_>>();
        let closed_reg = kr.close();
        ps.into_iter()
            .map(|p| p.create_signer(closed_reg.clone()).unwrap())
            .collect()
    }

    /// Generate a vector of stakes that should sum to `honest_stake`
    /// when ignoring the indices in `adversaries`
    fn arb_honest_for_adversaries(
        num_parties: usize,
        honest_stake: Stake,
        adversaries: HashMap<usize, Stake>,
    ) -> impl Strategy<Value = Vec<Stake>> {
        vec(1..honest_stake, num_parties).prop_map(move |parties| {
            let honest_sum = parties.iter().enumerate().fold(0, |acc, (i, s)| {
                if !adversaries.contains_key(&i) {
                    acc + s
                } else {
                    acc
                }
            });

            parties
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if let Some(a) = adversaries.get(&i) {
                        *a
                    } else {
                        (*s * honest_stake) / honest_sum
                    }
                })
                .collect()
        })
    }

    /// Generate a vector of `num_parties` stakes summing to `num_parties * total_stake`,
    /// plus a subset S of 0..num_parties such that the sum of the stakes at indices
    /// in S is adversary_stake * N
    fn arb_parties_with_adversaries(
        num_parties: usize,
        num_adversaries: usize,
        total_stake: Stake,
        adversary_stake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        hash_map(0..num_parties, 1..total_stake, num_adversaries).prop_flat_map(
            move |adversaries| {
                let adversary_sum: Stake = adversaries.values().sum();
                let adversaries_normed = adversaries
                    .iter()
                    .map(|(a, stake)| (*a, (stake * adversary_stake) / adversary_sum))
                    .collect();

                let adversaries = adversaries.into_keys().collect();
                (
                    Just(adversaries),
                    arb_honest_for_adversaries(
                        num_parties,
                        total_stake - adversary_stake,
                        adversaries_normed,
                    ),
                )
            },
        )
    }

    fn find_signatures(msg: &[u8], ps: &[Signer<D>], is: &[usize]) -> Vec<SingleSignature> {
        let mut sigs = Vec::new();
        for i in is {
            if let Some(sig) = ps[*i].sign(msg) {
                sigs.push(sig);
            }
        }
        sigs
    }

    /// Pick N between min and max, and then
    /// generate a vector of N stakes summing to N * tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake * N
    fn arb_parties_adversary_stake(
        min: usize,
        max: usize,
        tstake: Stake,
        astake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        (min..max)
            .prop_flat_map(|n| (Just(n), 1..=n / 2))
            .prop_flat_map(move |(n, nadv)| {
                arb_parties_with_adversaries(n, nadv, tstake * n as Stake, astake * n as Stake)
            })
    }

    #[derive(Debug)]
    struct ProofTest {
        msig: Result<Sig, AggregationError>,
        clerk: Clerk<D>,
        msg: [u8; 16],
    }
    /// Run the protocol up to aggregation. This will produce a valid aggregation of signatures.
    /// The following tests mutate this aggregation so that the proof is no longer valid.
    fn arb_proof_setup(max_parties: usize) -> impl Strategy<Value = ProofTest> {
        any::<[u8; 16]>().prop_flat_map(move |msg| {
            (2..max_parties).prop_map(move |n| {
                let params = Parameters {
                    m: 5,
                    k: 5,
                    phi_f: 1.0,
                };
                let ps = setup_equal_parties(params, n);
                let clerk = Clerk::new_clerk_from_signer(&ps[0]);

                let all_ps: Vec<usize> = (0..n).collect();
                let sigs = find_signatures(&msg, &ps, &all_ps);

                let msig = clerk.aggregate_signatures(&sigs, &msg);
                ProofTest { msig, clerk, msg }
            })
        })
    }

    fn with_proof_mod<F>(mut tc: ProofTest, f: F)
    where
        F: Fn(&mut Sig, &mut Clerk<D>, &mut [u8; 16]),
    {
        match tc.msig {
            Ok(mut aggr) => {
                f(&mut aggr, &mut tc.clerk, &mut tc.msg);
                assert!(
                    aggr.verify(
                        &tc.msg,
                        &tc.clerk.compute_aggregate_verification_key(),
                        &tc.clerk.params
                    )
                    .is_err()
                )
            }
            Err(e) => unreachable!("Reached an unexpected error: {:?}", e),
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that `dedup_sigs_for_indices` only takes valid signatures.
        fn test_dedup(msg in any::<[u8; 16]>()) {
            let false_msg = [1u8; 20];
            let params = Parameters { m: 1, k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, 1);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);
            let avk = clerk.compute_aggregate_verification_key();
            let mut sigs = Vec::with_capacity(2);

            if let Some(sig) = ps[0].sign(&false_msg) {
                sigs.push(sig);
            }

            if let Some(sig) = ps[0].sign(&msg) {
                sigs.push(sig);
            }

            let sig_reg_list = sigs
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: clerk.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

            let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(&msg);
            let dedup_result = BasicVerifier::select_valid_signatures_for_k_indices(
                &clerk.closed_reg.total_stake,
                &params,
                &msgp,
                &sig_reg_list,
            );
            assert!(dedup_result.is_ok(), "dedup failure {dedup_result:?}");
            for passed_sigs in dedup_result.unwrap() {
                let verify_result = passed_sigs.sig.verify(&params, &ps[0].get_verification_key(), &ps[0].get_stake(), &avk, &msg);
                assert!(verify_result.is_ok(), "verify {verify_result:?}");
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that when a quorum is found, the aggregate signature can be verified by anyone with
        /// access to the avk and the parameters.
        fn test_aggregate_sig(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = Parameters { m, k, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let msig = clerk.aggregate_signatures(&sigs, &msg);

            match msig {
                Ok(aggr) => {
                    let verify_result = aggr.verify(&msg, &clerk.compute_aggregate_verification_key(), &params);
                    assert!(verify_result.is_ok(), "Verification failed: {verify_result:?}");
                }
                Err(AggregationError::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k || k == params.k),
                Err(AggregationError::UsizeConversionInvalid) =>
                    unreachable!()
            }
        }

        #[test]
        /// Test that batch verification of certificates works
        fn batch_verify(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..4,
                              seed in any::<[u8;32]>(),
                              batch_size in 2..10,
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut aggr_avks = Vec::new();
            let mut aggr_stms = Vec::new();
            let mut batch_msgs = Vec::new();
            let mut batch_params = Vec::new();
            for _ in 0..batch_size {
                let mut msg = [0u8; 32];
                rng.fill_bytes(&mut msg);
                let params = Parameters { m, k, phi_f: 0.95 };
                let ps = setup_equal_parties(params, nparties);
                let clerk = Clerk::new_clerk_from_signer(&ps[0]);

                let all_ps: Vec<usize> = (0..nparties).collect();
                let sigs = find_signatures(&msg, &ps, &all_ps);
                let msig = clerk.aggregate_signatures(&sigs, &msg);

                match msig {
                    Ok(aggr) => {
                        aggr_avks.push(clerk.compute_aggregate_verification_key());
                        aggr_stms.push(aggr);
                        batch_msgs.push(msg.to_vec());
                        batch_params.push(params);
                    }
                    Err(AggregationError::NotEnoughSignatures(_n, _k)) => {
                        assert!(sigs.len() < params.k as usize)
                    }
                    Err(AggregationError::UsizeConversionInvalid) => unreachable!(),
                }
            }

            assert!(AggregateSignature::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params).is_ok());

            let mut msg = [0u8; 32];
            rng.fill_bytes(&mut msg);
            let params = Parameters { m, k, phi_f: 0.8 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let fake_msig = clerk.aggregate_signatures(&sigs, &msg);

            aggr_stms[0] = fake_msig.unwrap();
            assert!(AggregateSignature::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params).is_err());
        }
    }

    proptest! {
        #[test]
        /// Test that when a party creates a signature it can be verified
        fn test_sig(msg in any::<[u8;16]>()) {
            let params = Parameters { m: 1, k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, 1);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);
            let avk = clerk.compute_aggregate_verification_key();

            if let Some(sig) = ps[0].sign(&msg) {
                assert!(sig.verify(&params, &ps[0].get_verification_key(), &ps[0].get_stake(), &avk, &msg).is_ok());
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]
        #[test]
        fn test_parameters_serialize_deserialize(m in any::<u64>(), k in any::<u64>(), phi_f in any::<f64>()) {
            let params = Parameters { m, k, phi_f };

            let bytes = params.to_bytes();
            let deserialised = Parameters::from_bytes(&bytes);
            assert!(deserialised.is_ok())
        }

        #[test]
        fn test_initializer_serialize_deserialize(seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let params = Parameters { m: 1, k: 1, phi_f: 1.0 };
            let stake = rng.next_u64();
            let initializer = Initializer::new(params, stake, &mut rng);

            let bytes = initializer.to_bytes();
            assert!(Initializer::from_bytes(&bytes).is_ok());

            let bytes = bincode::serde::encode_to_vec(&initializer, bincode::config::legacy()).unwrap();
            assert!(bincode::serde::decode_from_slice::<Initializer,_>(&bytes, bincode::config::legacy()).is_ok())
        }

        #[test]
        fn test_sig_serialize_deserialize(msg in any::<[u8;16]>()) {
            let params = Parameters { m: 1, k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, 1);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);
            let avk = clerk.compute_aggregate_verification_key();

            if let Some(sig) = ps[0].sign(&msg) {
                let bytes = sig.to_bytes();
                let sig_deser = SingleSignature::from_bytes::<D>(&bytes).unwrap();
                assert!(sig_deser.verify(&params, &ps[0].get_verification_key(), &ps[0].get_stake(), &avk, &msg).is_ok());

                let encoded = bincode::serde::encode_to_vec(&sig, bincode::config::legacy()).unwrap();
                let (decoded,_) = bincode::serde::decode_from_slice::<SingleSignature,_>(&encoded, bincode::config::legacy()).unwrap();
                assert!(decoded.verify(&params, &ps[0].get_verification_key(), &ps[0].get_stake(), &avk, &msg).is_ok());
            }
        }

        #[test]
        fn test_multisig_serialize_deserialize(nparties in 2_usize..10,
                                          msg in any::<[u8;16]>()) {
            let params = Parameters { m: 10, k: 5, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let sigs = find_signatures(&msg, &ps, &all_ps);
            let msig = clerk.aggregate_signatures(&sigs, &msg);
            if let Ok(aggr) = msig {
                    let bytes: Vec<u8> = aggr.to_bytes();
                    let aggr2 = AggregateSignature::from_bytes(&bytes).unwrap();
                    assert!(aggr2.verify(&msg, &clerk.compute_aggregate_verification_key(), &params).is_ok());

                    let encoded = bincode::serde::encode_to_vec(&aggr, bincode::config::legacy()).unwrap();
                    let (decoded,_) = bincode::serde::decode_from_slice::<AggregateSignature<D>,_>(&encoded, bincode::config::legacy()).unwrap();
                    assert!(decoded.verify(&msg, &clerk.compute_aggregate_verification_key(), &params).is_ok());
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        /// Test that when the adversaries do not hold sufficient stake, they can not form a quorum
        fn test_adversary_quorum(
            (adversaries, parties) in arb_parties_adversary_stake(8, 30, 16, 4),
            msg in any::<[u8;16]>(),
        ) {
            // Test sanity check:
            // Check that the adversarial party has less than 40% of the total stake.
            let (good, bad) = parties.iter().enumerate().fold((0,0), |(acc1, acc2), (i, st)| {
                if adversaries.contains(&i) {
                    (acc1, acc2 + *st)
                } else {
                    (acc1 + *st, acc2)
                }
            });
            assert!(bad as f64 / ((good + bad) as f64) < 0.4);

            let params = Parameters { m: 2642, k: 357, phi_f: 0.2 }; // From Table 1
            let ps = setup_parties(params, parties);

            let sigs =  find_signatures(&msg, &ps, &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

            let clerk = Clerk::new_clerk_from_signer(&ps[0]);

            let msig = clerk.aggregate_signatures(&sigs, &msg);
            match msig {
                Err(AggregationError::NotEnoughSignatures(n, k)) =>
                    assert!(n < params.k && params.k == k),
                _ =>
                    unreachable!(),
            }
        }
    }

    proptest! {
        // Each of the tests below corresponds to falsifying a conjunct in the
        // definition of a valid signature
        #[test]
        fn test_invalid_proof_quorum(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |_aggr, clerk, _msg| {
                clerk.params.k += 7;
            })
        }
        // todo: fn test_invalid_proof_individual_sig
        #[test]
        fn test_invalid_proof_index_bound(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |_aggr, clerk, _msg| {
                clerk.params.m = 1;
            })
        }
        #[test]
        fn test_invalid_proof_index_unique(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |aggr, clerk, _msg| {
                if let AggregateSignature::Concatenation(concatenation_proof) = aggr {
                    for sig_reg in concatenation_proof.signatures.iter_mut() {
                        for index in sig_reg.sig.indexes.iter_mut() {
                        *index %= clerk.params.k - 1
                        }
                    }
                }else{
                    panic!("Unexpected aggregate signature type");
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |aggr, _, _msg| {
                if let AggregateSignature::Concatenation(concatenation_proof) = aggr {
                   let p = concatenation_proof.batch_proof.clone();
                    let mut index_list = p.indices.clone();
                    let values = p.values;
                    let batch_proof = {
                        index_list[0] += 1;
                        MerkleBatchPath {
                            values,
                            indices: index_list,
                            hasher: Default::default()
                        }
                    };
                    concatenation_proof.batch_proof = batch_proof;
                }else{
                    panic!("Unexpected aggregate signature type");
                }
            })
        }
    }

    // ---------------------------------------------------------------------
    // Basic verifier
    // ---------------------------------------------------------------------
    fn setup_equal_core_parties(
        params: Parameters,
        nparties: usize,
    ) -> (Vec<Initializer>, Vec<(BlsVerificationKey, Stake)>) {
        let stake = vec![1; nparties];
        setup_core_parties(params, stake)
    }

    fn setup_core_parties(
        params: Parameters,
        stake: Vec<Stake>,
    ) -> (Vec<Initializer>, Vec<(BlsVerificationKey, Stake)>) {
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = ChaCha20Rng::from_seed(trng.random());

        let ps = stake
            .into_iter()
            .map(|stake| Initializer::new(params, stake, &mut rng))
            .collect::<Vec<Initializer>>();

        let public_signers = ps
            .iter()
            .map(|s| (s.pk.vk, s.stake))
            .collect::<Vec<(BlsVerificationKey, Stake)>>();

        (ps, public_signers)
    }

    fn find_core_signatures(
        msg: &[u8],
        ps: &[Signer<D>],
        total_stake: Stake,
        is: &[usize],
    ) -> Vec<SingleSignature> {
        let mut sigs = Vec::new();
        for i in is {
            if let Some(sig) = ps[*i].basic_sign(msg, total_stake) {
                sigs.push(sig);
            }
        }
        sigs
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn test_core_verifier(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {

            let params = Parameters { m, k, phi_f: 0.2 };
            let (initializers, public_signers) = setup_equal_core_parties(params, nparties);
            let all_ps: Vec<usize> = (0..nparties).collect();

            let core_verifier = BasicVerifier::new(&public_signers);

            let signers = initializers
                .into_iter()
                .filter_map(|s| s.create_basic_signer(&core_verifier.eligible_parties))
                .collect::<Vec<Signer<D>>>();

            let signatures = find_core_signatures(&msg, &signers, core_verifier.total_stake, &all_ps);

            let verify_result = core_verifier.verify(&signatures, &params, &msg);

            match verify_result{
                Ok(_) => {
                    assert!(verify_result.is_ok(), "Verification failed: {verify_result:?}");
                }
                Err(CoreVerifierError::NoQuorum(nr_indices, _k)) => {
                    assert!((nr_indices) < params.k);
                }
                Err(CoreVerifierError::IndexNotUnique) => unreachable!(),
                _ => unreachable!(),
            }
        }

        #[test]
        fn test_total_stake_core_verifier(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,) {
            let params = Parameters { m, k, phi_f: 0.2 };
            let (_initializers, public_signers) = setup_equal_core_parties(params, nparties);
            let core_verifier = BasicVerifier::new(&public_signers);
            assert_eq!(nparties as u64, core_verifier.total_stake, "Total stake expected: {}, got: {}.", nparties, core_verifier.total_stake);
        }
    }
}
