use super::{Stake, PartyId, Index, Path, ev_lt_phi};
use crate::key_reg::KeyReg;
use crate::msp::{Msp, MspMvk, MspSig, MspPk, MspSk};
use crate::merkle_tree::MerkleTree;
use crate::proof::Proof;

#[derive(Clone, Debug, Copy)]
pub struct StmParameters {
    /// Security parameter, upper bound on indices
    pub m: u64,

    /// Quorum parameter
    pub k: u64,

    /// `f` in phi(w) = 1 - (1 - f)^w
    pub phi_f: f64,
}

pub struct StmParty {
    party_id: PartyId,
    stake: Stake,
    avk: Option<MerkleTree>,
    sk: Option<MspSk>,
    pk: Option<MspPk>,
    reg: Option<Vec<Option<(MspPk, Stake)>>>, // map from PID -> (PK,Stake)
    total_stake: Option<Stake>,
    params: StmParameters
}

#[derive(Clone, Debug)]
pub struct StmSig {
    pub sigma: MspSig,
    pub pk: MspPk,
    pub party: PartyId,
    pub stake: Stake,
    pub path: Path,
}

#[derive(Clone)]
pub struct StmMultiSig<P> {
    ivk: MspMvk,
    mu: MspSig,
    proof: P,
}

#[derive(Debug)]
pub enum AggregationFailure {
    VerifyFailed,
    DuplicateIndex,
}

impl StmParty {
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
            reg: None,
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
        self.reg = Some(kr.retrieve_all());
        // get total stake
        self.total_stake = Some(self.reg.as_ref().unwrap().iter().filter_map(|p| p.map(|(_,s)|s)).sum());
    }

    // Creating a MerkleTree is expensive. Only do it if you have to.
    pub fn create_avk(&mut self) {
        let avk: MerkleTree = MerkleTree::create(self.reg.as_ref().unwrap());
        self.avk = Some(avk);
    }

    /////////////////////
    // Operation phase //
    /////////////////////
    pub fn eligibility_check(&self, msg: &[u8], index: Index) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let msgp = self.avk.as_ref().unwrap().concat_with_msg(msg);
        let sigma = Msp::sig(self.sk.as_ref().unwrap(), &msgp);
        let ev = Msp::eval(&msgp, index, &sigma);
        ev_lt_phi(self.params.phi_f, ev, self.stake, self.total_stake.unwrap())
    }

    pub fn create_sig(&self, msg: &[u8], index: Index) -> Option<StmSig> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = self.avk.as_ref().unwrap().concat_with_msg(msg);
            let sigma = Msp::sig(self.sk.as_ref().unwrap(), &msgp);
            let path = self.avk.as_ref().unwrap().get_path(self.party_id);
            let pk = self.pk.as_ref().unwrap().clone();
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

    pub fn verify(&self, sig: StmSig, index: Index, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        let msgp = avk.concat_with_msg(msg);
        let ev = Msp::eval(&msgp, index, &sig.sigma);
        if !ev_lt_phi(self.params.phi_f, ev, sig.stake, self.total_stake.unwrap()) ||
            !avk.check(&(sig.pk.clone(), sig.stake), sig.party, &sig.path)
        {
            return false;
        }
        Msp::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate<P:Proof>(&self, sigs: &[StmSig], indices: &[Index], msg: &[u8])
        -> Result<StmMultiSig<P>, AggregationFailure>
    {
        let avk = self.avk.as_ref().unwrap();
        let msgp = avk.concat_with_msg(msg);
        let mut seen_indices = std::collections::HashSet::new();
        let mut evals = Vec::new();
        for (sig, ix) in sigs.iter().zip(indices.iter()) {
            if !self.verify(sig.clone(), *ix, msg) {
                return Err(AggregationFailure::VerifyFailed);
            } else if seen_indices.contains(ix)
            {
                return Err(AggregationFailure::DuplicateIndex);
            }
            seen_indices.insert(*ix);
            evals.push(Msp::eval(&msgp, *ix, &sig.sigma));
        }
        let mvks = sigs.iter().map(|sig| sig.pk.mvk).collect::<Vec<_>>();
        let sigmas = sigs.iter().map(|sig| sig.sigma).collect::<Vec<_>>();
        let ivk = Msp::aggregate_keys(&mvks);
        let mu = Msp::aggregate_sigs(msg, &sigmas);
        let proof = P::prove(avk, &ivk, msg, &sigs, &indices, &evals);
        Ok(StmMultiSig {
            ivk,
            mu,
            proof,
        })
    }

    pub fn verify_aggregate<P:Proof>(&self, msig: &StmMultiSig<P>, msg: &[u8]) -> bool {
        let avk = self.avk.as_ref().unwrap();
        if !msig.proof.verify(&self.params, self.total_stake.unwrap(), avk, &msig.ivk, msg) {
            return false;
        }
        let msgp = avk.concat_with_msg(msg);
        Msp::aggregate_ver(&msgp, &msig.ivk, &msig.mu)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashSet, HashMap};
    use proptest::prelude::*;
    use crate::proof::ConcatProof;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmParty> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters,
                     stake: Vec<Stake>) -> Vec<StmParty> {
        let mut kr = KeyReg::new();
        let mut ps = stake.iter()
                          .enumerate()
                          .map(|(pid, stake)| {
                              let mut p = StmParty::setup(params, pid, *stake);
                              p.register(&mut kr);
                              p
                          }).collect::<Vec<StmParty>>();
        for p in ps.iter_mut() {
            p.retrieve_all(&kr);
        }
        ps
    }

    fn arb_num_parties(min: u32, max: u32) -> impl Strategy<Value = usize> {
        let min_height = (min as f64).log2().ceil() as u32;
        let max_height = (max as f64).log2().ceil() as u32;
        (min_height..max_height)
            .prop_map(|h| (2 as usize).pow(h))
    }

    fn arb_honest_for_adversaries(num_parties: usize,
                                  honest_stake: Stake,
                                  adversaries: HashMap<usize, Stake>) -> impl Strategy<Value = Vec<Stake>> {
        // let honest = (0..num_parties).filter(|i| !adversaries.contains_key(i));
        proptest::collection::vec(1..honest_stake, num_parties).prop_map(move |parties| {
            let honest_sum = parties
                .iter()
                .enumerate()
                .fold(0, |acc, (i, s)| if !adversaries.contains_key(&i) { acc + s } else { acc });

            parties.iter().enumerate().map(|(i, s)| {
                if let Some(a) = adversaries.get(&i) {
                    *a
                } else {
                    (*s * honest_stake)/honest_sum
                }
            }).collect()
        })
    }

    fn arb_parties_with_adversaries(num_parties: usize,
                                    num_adversaries: usize,
                                    total_stake: Stake,
                                    adversary_stake: Stake,
    ) -> impl Strategy<Value = (HashSet<usize>, Vec<Stake>)> {
        proptest::collection::hash_map(0..num_parties,
                                       1..total_stake,
                                       num_adversaries)
            .prop_flat_map(move |adversaries| {
                let adversary_sum: Stake = adversaries
                                           .values()
                                           .sum();
                let adversaries_normed = adversaries.iter().map(|(a, stake)| {
                    (*a, (stake*adversary_stake)/adversary_sum)
                }).collect();

                let adversaries = adversaries.into_keys().collect();
                (Just(adversaries), arb_honest_for_adversaries(num_parties, total_stake-adversary_stake, adversaries_normed))
            })
    }

    fn arb_num_parties_and_index(min:u32, max: u32) -> impl Strategy <Value = (usize, usize)> {
        arb_num_parties(min, max)
            .prop_flat_map(|n| {
                (Just(n), 0..n)
            })
    }

    fn find_signatures(m: u64, k: u64, msg: &[u8], ps: &mut [StmParty], is: &[usize])
                          -> (Vec<Index>, Vec<StmSig>) {
        let mut ixs = Vec::new();
        let mut sigs = Vec::new();
        for ix in 1..m {
            for i in is {
                if let Some(sig) = ps[*i].create_sig(&msg, ix) {
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
            p.create_avk();

            for index in 1..nparties {
                if let Some(sig) = p.create_sig(&msg, index as u64) {
                    assert!(p.verify(sig, index as u64, &msg));
                }
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        /// Test that when a quorum is found, the aggregate signature can be verified
        fn test_aggregate_sig(nparties in arb_num_parties(2, 16),
                              (m, k) in (10_u64..20).prop_flat_map(|mm| (Just(mm), 1_u64..5)),
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m: m, k: k, phi_f: 0.2 };
            let mut ps = setup_equal_parties(params, nparties);
            ps.iter_mut().for_each(StmParty::create_avk);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let (ixs, sigs) = find_signatures(m, k, &msg, &mut ps, &all_ps);

            if sigs.len() >= params.k as usize {
                let msig = ps[0].aggregate::<ConcatProof>(&sigs[0..k as usize], &ixs[0..k as usize], &msg).unwrap();
                assert!(ps[1].verify_aggregate(&msig, &msg));
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        /// Test that when the adversaries do not hold sufficient stake, they can not form a quorum
        fn test_adversary_quorum(
            (adversaries, parties) in
                arb_num_parties(8,64)
                .prop_flat_map(|n| (Just (n), 1..=n/2, Just(16*n as Stake), Just(4*n as Stake)))
                .prop_flat_map(|(n, nadv, tstake, astake)|
                               arb_parties_with_adversaries(n, nadv, tstake, astake)),
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
            let aggregator = i % parties.len();
            let verifier   = j % parties.len();

            let params = StmParameters { m: 2642, k: 357, phi_f: 0.2 }; // From Table 1
            let mut ps = setup_parties(params, parties);

            for a in &adversaries {
                ps[*a].create_avk();
            }
            if !adversaries.contains(&aggregator) {
                ps[aggregator].create_avk();
            }
            if !adversaries.contains(&verifier) {
                ps[verifier].create_avk();
            }

            let (ixs, sigs) = find_signatures(params.m,
                                              params.k,
                                              &msg,
                                              &mut ps,
                                              &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

            let msig = ps[aggregator].aggregate::<ConcatProof>(&sigs, &ixs, &msg)
                                     .expect("Aggregate failed");
            assert!(!ps[verifier].verify_aggregate(&msig, &msg));

        }
    }
}
