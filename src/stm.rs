//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.

use super::{ev_lt_phi, Index, PartyId, Path, Stake};
use crate::key_reg::KeyReg;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk, MspPk, MspSig, MspSk};
use crate::proof::Proof;

/// Used to set protocol parameters.
#[derive(Clone, Debug, Copy, PartialEq)]
pub struct StmParameters {
    /// Security parameter, upper bound on indices
    pub m: u64,

    /// Quorum parameter
    pub k: u64,

    /// `f` in phi(w) = 1 - (1 - f)^w
    pub phi_f: f64,
}

/// Initializer for `StmSigner`.
pub struct StmInitializer {
    party_id: PartyId,
    stake: Stake,
    avk: Option<MerkleTree>,
    sk: Option<MspSk>,
    pk: Option<MspPk>,
    total_stake: Option<Stake>,
    params: StmParameters,
}

/// Participant in the protocol. Can sign messages.
pub struct StmSigner {
    party_id: PartyId,
    stake: Stake,
    params: StmParameters,
    total_stake: Stake,
    avk: MerkleTree,
    sk: MspSk,
    pk: MspPk,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
pub struct StmClerk {
    avk: MerkleTree,
    params: StmParameters,
    total_stake: Stake,
}

/// Signature created by a single party who has won the lottery.
#[derive(Clone, Debug)]
pub struct StmSig {
    pub sigma: MspSig,
    pub pk: MspPk,
    pub party: PartyId,
    pub stake: Stake,
    pub path: Path,
}

/// Aggregated signature of many parties.
/// Contains proof that it is well-formed.
#[derive(Clone)]
pub struct StmMultiSig<P> {
    ivk: MspMvk,
    mu: MspSig,
    proof: P,
}

/// Error types for aggregation.
#[derive(Debug)]
pub enum AggregationFailure {
    VerifyFailed,
    DuplicateIndex,
}

impl StmInitializer {
    //////////////////////////
    // Initialization phase //
    //////////////////////////
    pub fn setup(params: StmParameters, party_id: PartyId, stake: Stake) -> Self {
        Self {
            party_id,
            stake,
            avk: None,
            sk: None,
            pk: None,
            total_stake: None,
            params,
        }
    }

    pub fn register(&mut self, kr: &mut KeyReg) {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = Msp::gen();
        self.sk = Some(sk);
        self.pk = Some(pk.clone());
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        let reg = kr.retrieve_all();
        self.avk = Some(MerkleTree::create(&reg));
        // get total stake
        self.total_stake = Some(reg.iter().filter_map(|p| p.map(|(_, s)| s)).sum());
    }

    pub fn finish(self) -> StmSigner {
        StmSigner {
            party_id: self.party_id,
            stake: self.stake,
            params: self.params,
            total_stake: self.total_stake.expect("total stake unknown"),
            avk: self.avk.expect("merkle tree not created"),
            sk: self.sk.expect("register not called"),
            pk: self.pk.expect("register not called"),
        }
    }
}

impl StmSigner {
    /////////////////////
    // Operation phase //
    /////////////////////
    pub fn eligibility_check(&self, msg: &[u8], index: Index) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let msgp = self.avk.concat_with_msg(msg);
        let sigma = Msp::sig(&self.sk, &msgp);
        let ev = Msp::eval(&msgp, index, &sigma);
        ev_lt_phi(self.params.phi_f, ev, self.stake, self.total_stake)
    }

    pub fn sign(&self, msg: &[u8], index: Index) -> Option<StmSig> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = self.avk.concat_with_msg(msg);
            let sigma = Msp::sig(&self.sk, &msgp);
            let path = self.avk.get_path(self.party_id);
            let pk = self.pk.clone();
            Some(StmSig {
                sigma,
                pk,
                party: self.party_id,
                stake: self.stake,
                path,
            })
        } else {
            None
        }
    }
}

impl StmClerk {
    pub fn new(params: StmParameters, avk: MerkleTree, total_stake: Stake) -> Self {
        Self {
            params,
            avk,
            total_stake,
        }
    }

    pub fn from_signer(signer: &StmSigner) -> Self {
        Self::new(signer.params, signer.avk.clone(), signer.total_stake)
    }

    pub fn verify_sig(&self, sig: &StmSig, index: Index, msg: &[u8]) -> bool {
        let msgp = self.avk.concat_with_msg(msg);
        let ev = Msp::eval(&msgp, index, &sig.sigma);
        if !ev_lt_phi(self.params.phi_f, ev, sig.stake, self.total_stake)
            || !self.avk.check(&(sig.pk.clone(), sig.stake), sig.party, &sig.path)
        {
            return false;
        }
        Msp::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate<P: Proof>(
        &self,
        sigs: &[StmSig],
        indices: &[Index],
        msg: &[u8],
    ) -> Result<StmMultiSig<P>, AggregationFailure> {
        let msgp = self.avk.concat_with_msg(msg);
        let mut seen_indices = std::collections::HashSet::new();
        let mut evals = Vec::new();
        for (sig, ix) in sigs.iter().zip(indices.iter()) {
            if !self.verify_sig(sig, *ix, msg) {
                return Err(AggregationFailure::VerifyFailed);
            } else if seen_indices.contains(ix) {
                return Err(AggregationFailure::DuplicateIndex);
            }
            seen_indices.insert(*ix);
            evals.push(Msp::eval(&msgp, *ix, &sig.sigma));
        }
        let mvks = sigs.iter().map(|sig| sig.pk.mvk).collect::<Vec<_>>();
        let sigmas = sigs.iter().map(|sig| sig.sigma).collect::<Vec<_>>();
        let ivk = Msp::aggregate_keys(&mvks);
        let mu = Msp::aggregate_sigs(msg, &sigmas);
        let proof = P::prove(&self.avk, &ivk, msg, &sigs, &indices, &evals);
        Ok(StmMultiSig { ivk, mu, proof })
    }

    pub fn verify_msig<P: Proof>(&self, msig: &StmMultiSig<P>, msg: &[u8]) -> bool {
        if !msig
            .proof
            .verify(&self.params, self.total_stake, &self.avk, &msig.ivk, msg)
        {
            return false;
        }
        let msgp = self.avk.concat_with_msg(msg);
        Msp::aggregate_ver(&msgp, &msig.ivk, &msig.mu)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::proof::ConcatProof;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use std::collections::{HashMap, HashSet};

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSigner> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSigner> {
        let mut kr = KeyReg::new();
        let ps = stake
            .iter()
            .enumerate()
            .map(|(pid, stake)| {
                let mut p = StmInitializer::setup(params, pid, *stake);
                p.register(&mut kr);
                p
            }).collect::<Vec<_>>();
        ps.into_iter().map(|mut p| {
            p.retrieve_all(&kr);
            p.finish()
        }).collect()
    }

    /// Pick an arbitrary power of 2 between min and max
    fn arb_num_parties(min: u32, max: u32) -> impl Strategy<Value = usize> {
        let min_height = (min as f64).log2().ceil() as u32;
        let max_height = (max as f64).log2().ceil() as u32;
        (min_height..max_height).prop_map(|h| (2 as usize).pow(h))
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

    /// Generate a vector of N stakes summing to N*tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake*N
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

    fn arb_num_parties_and_index(min: u32, max: u32) -> impl Strategy<Value = (usize, usize)> {
        arb_num_parties(min, max).prop_flat_map(|n| (Just(n), 0..n))
    }

    fn find_signatures(
        m: u64,
        k: u64,
        msg: &[u8],
        ps: &mut [StmSigner],
        is: &[usize],
    ) -> (Vec<Index>, Vec<StmSig>) {
        let mut ixs = Vec::new();
        let mut sigs = Vec::new();
        for ix in 1..m {
            for i in is {
                if let Some(sig) = ps[*i].sign(&msg, ix) {
                    sigs.push(sig);
                    ixs.push(ix);
                    break;
                }
            }
            if ixs.len() == k as usize {
                break;
            }
        }

        (ixs, sigs)
    }

    proptest! {
        #[test]
        /// Test that when a party creates a signature it can be verified
        fn test_sig((nparties, p_i) in arb_num_parties_and_index(2, 32),
                    msg in any::<[u8;16]>()) {
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 0.2 };
            let mut ps = setup_equal_parties(params, nparties);
            let p = &mut ps[p_i];
            let clerk = StmClerk::from_signer(&p);

            for index in 1..nparties {
                if let Some(sig) = p.sign(&msg, index as u64) {
                    assert!(clerk.verify_sig(&sig, index as u64, &msg));
                }
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that when a quorum is found, the aggregate signature can be verified
        fn test_aggregate_sig(nparties in arb_num_parties(2, 16),
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m: m, k: k, phi_f: 0.2 };
            let mut ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let (ixs, sigs) = find_signatures(m, k, &msg, &mut ps, &all_ps);

            if sigs.len() >= params.k as usize {
<<<<<<< HEAD
                let msig = ps[0].aggregate::<ConcatProof>(&sigs[0..k as usize], &ixs[0..k as usize], &msg).unwrap();
                assert!(ps[1].verify_aggregate(&msig, &msg));
=======
                let msig = clerk.aggregate(&sigs[0..k as usize], &ixs[0..k as usize], &msg).unwrap();
                assert!(clerk.verify_msig(&msig, &msg));
>>>>>>> refactor-stm-api
            }
        }
    }

    /// Pick a power of 2 N between min and max, and then
    /// generate a vector of N stakes summing to N*tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake*N
    fn arb_parties_adversary_stake(
        min: u32,
        max: u32,
        tstake: Stake,
        astake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        arb_num_parties(min, max)
            .prop_flat_map(|n| (Just(n), 1..=n / 2))
            .prop_flat_map(move |(n, nadv)| {
                arb_parties_with_adversaries(n, nadv, tstake * n as Stake, astake * n as Stake)
            })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        /// Test that when the adversaries do not hold sufficient stake, they can not form a quorum
        fn test_adversary_quorum(
            (adversaries, parties) in arb_parties_adversary_stake(8, 64, 16, 4),
            msg in any::<[u8;16]>(),
            i in any::<usize>(), j in any::<usize>(),
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

            let params = StmParameters { m: 2642, k: 357, phi_f: 0.2 }; // From Table 1
            let mut ps = setup_parties(params, parties);

            let (ixs, sigs) = find_signatures(params.m,
                                              params.k,
                                              &msg,
                                              &mut ps,
                                              &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

<<<<<<< HEAD
            let msig = ps[aggregator].aggregate::<ConcatProof>(&sigs, &ixs, &msg)
                                     .expect("Aggregate failed");
            assert!(!ps[verifier].verify_aggregate(&msig, &msg));
=======
            let clerk = StmClerk::from_signer(&ps[0]);

            let msig = clerk.aggregate(&sigs, &ixs, &msg).expect("Aggregate failed");
            assert!(!clerk.verify_msig(&msig, &msg));
>>>>>>> refactor-stm-api
        }
    }
}
