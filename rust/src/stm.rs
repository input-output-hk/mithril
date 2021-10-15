//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.

use super::{concat_avk_with_msg, ev_lt_phi, Index, PartyId, Path, Stake};
use crate::key_reg::{KeyReg, RegParty};
use crate::merkle_tree::{MTHashLeaf, MerkleTree};
use crate::mithril_proof::{MithrilProof, Statement, Witness};
use crate::msp::{Msp, MspMvk, MspPk, MspSig, MspSk};
use crate::proof::ProverEnv;
use ark_ec::PairingEngine;
use ark_ff::ToConstraintField;
use rand::Rng;
use std::collections::HashMap;
use std::convert::From;
use std::rc::Rc;
use std::iter::FromIterator;

/// The values that are represented in the Merkle Tree.
#[derive(Debug, Clone, Copy)]
pub struct MTValue<PE: PairingEngine>(pub MspMvk<PE>, pub Stake);

/// Used to set protocol parameters.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmParameters {
    /// Security parameter, upper bound on indices
    pub m: u64,

    /// Quorum parameter
    pub k: u64,

    /// `f` in phi(w) = 1 - (1 - f)^w
    pub phi_f: f64,
}

/// Initializer for `StmSigner`.
#[derive(Debug, Clone)]
pub struct StmInitializer<H, PE>
where
    H: MTHashLeaf<MTValue<PE>>,
    PE: PairingEngine,
{
    party_id: PartyId,
    stake: Stake,
    params: StmParameters,
    avk: Option<MerkleTree<MTValue<PE>, H>>,
    avk_indices: Option<HashMap<PartyId, usize>>,
    sk: Option<MspSk<PE>>,
    pk: Option<MspPk<PE>>,
    total_stake: Option<Stake>,
}

/// Participant in the protocol. Can sign messages.
#[derive(Debug, Clone)]
pub struct StmSigner<H, PE>
where
    H: MTHashLeaf<MTValue<PE>>,
    PE: PairingEngine,
{
    party_id: PartyId,
    avk_idx: usize,
    stake: Stake,
    params: StmParameters,
    avk: MerkleTree<MTValue<PE>, H>,
    sk: MspSk<PE>,
    pk: MspPk<PE>,
    total_stake: Stake,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
#[derive(Debug, Clone)]
pub struct StmClerk<H, PE, E>
where
    H: MTHashLeaf<MTValue<PE>>,
    PE: PairingEngine,
    E: ProverEnv,
{
    avk: Rc<MerkleTree<MTValue<PE>, H>>,
    params: StmParameters,
    total_stake: Stake,
    proof_env: E,
    proof_key: E::ProvingKey,
    verif_key: E::VerificationKey,
}

/// Signature created by a single party who has won the lottery.
#[derive(Debug, Clone)]
pub struct StmSig<PE: PairingEngine, F> {
    pub sigma: MspSig<PE>,
    pub pk: MspPk<PE>,
    pub party: PartyId,
    pub stake: Stake,
    pub path: Path<F>,
}

/// Aggregated signature of many parties.
/// Contains proof that it is well-formed.
#[derive(Debug, Clone)]
pub struct StmMultiSig<PE, Proof>
where
    PE: PairingEngine,
{
    ivk: MspMvk<PE>,
    mu: MspSig<PE>,
    proof: Proof,
}

/// Error types for aggregation.
#[derive(Debug, Clone, Copy)]
pub enum AggregationFailure {
    NotEnoughSignatures(usize),
    /// How many signatures we got
    VerifyFailed,
}

impl<H, PE> StmInitializer<H, PE>
where
    PE: PairingEngine,
    MspPk<PE>: std::hash::Hash,
    H: MTHashLeaf<MTValue<PE>>,
{
    //////////////////////////
    // Initialization phase //
    //////////////////////////
    pub fn setup(params: StmParameters, party_id: PartyId, stake: Stake) -> Self {
        Self {
            party_id,
            stake,
            avk_indices: None,
            avk: None,
            sk: None,
            pk: None,
            total_stake: None,
            params,
        }
    }

    pub fn register<R>(&mut self, rng: &mut R, kr: &mut KeyReg<PE>)
    where
        R: Rng + rand::CryptoRng + ?Sized,
    {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = Msp::gen(rng);
        self.sk = Some(sk);
        self.pk = Some(pk);
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg<PE>) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        let reg: Vec<RegParty<PE>> = kr.retrieve_all();

        let mtvals = reg.iter().map(|rp| MTValue(rp.pk.mvk, rp.stake)).collect::<Vec<_>>();
        self.avk = Some(MerkleTree::create(&mtvals));
        self.avk_indices = Some(HashMap::from_iter(reg.iter().enumerate().map(|(i, rp)| (rp.party_id, i))));
        // get total stake
        self.total_stake = Some(mtvals.iter().map(|s| s.1).sum());
    }

    pub fn finish(self) -> StmSigner<H, PE> {
        let indices = self.avk_indices.expect("registered party indices unknown");
        let my_index = indices
            .get(&self.party_id)
            .expect(&format!("party unkown: {}", self.party_id));

        StmSigner {
            party_id: self.party_id,
            avk_idx: *my_index,
            stake: self.stake,
            params: self.params,
            total_stake: self.total_stake.expect("total stake unknown"),
            avk: self.avk.expect("merkle tree not created"),
            sk: self.sk.expect("register not called"),
            pk: self.pk.expect("register not called"),
        }
    }
}

impl<H, PE> StmSigner<H, PE>
where
    H: MTHashLeaf<MTValue<PE>>,
    PE: PairingEngine,
{
    /////////////////////
    // Operation phase //
    /////////////////////
    pub fn eligibility_check(&self, msg: &[u8], index: Index) -> bool {
        // let msg' <- AVK || msg
        // sigma <- MSP.Sig(msk, msg')
        // ev <- MSP.Eval(msg', index, sigma)
        // return 1 if ev < phi(stake) else return 0
        let msgp = concat_avk_with_msg(&self.avk, msg);
        let sigma = Msp::sig(&self.sk, &msgp);
        let ev = Msp::eval(&msgp, index, &sigma);
        ev_lt_phi(self.params.phi_f, ev, self.stake, self.total_stake)
    }

    pub fn sign(&self, msg: &[u8], index: Index) -> Option<StmSig<PE, H::F>> {
        if self.eligibility_check(msg, index) {
            // msg' <- AVK||msg
            // sigma <- MSP.Sig(msk,msg')
            // pi = (sigma, reg_i, i, p_i) where
            //      p_i is the users path inside the merkle tree AVK
            //      reg_i is (mvk_i, stake_i)
            // return pi
            let msgp = concat_avk_with_msg(&self.avk, msg);
            let sigma = Msp::sig(&self.sk, &msgp);
            let path = self.avk.get_path(self.avk_idx);
            Some(StmSig {
                sigma,
                pk: self.pk,
                party: self.avk_idx,
                stake: self.stake,
                path,
            })
        } else {
            None
        }
    }
}

impl<H, PE, E: ProverEnv> StmClerk<H, PE, E>
where
    E: ProverEnv,
    H: MTHashLeaf<MTValue<PE>> + Clone,
    PE: PairingEngine,
    PE::G1Projective: ToConstraintField<PE::Fq>,
{
    pub fn new(
        params: StmParameters,
        proof_env: E,
        avk: MerkleTree<MTValue<PE>, H>,
        total_stake: Stake,
    ) -> Self {
        let (pk, vk) = proof_env.setup();
        Self {
            params,
            avk: Rc::new(avk),
            total_stake,
            proof_env,
            proof_key: pk,
            verif_key: vk,
        }
    }

    pub fn from_signer(signer: &StmSigner<H, PE>, proof_env: E) -> Self {
        Self::new(
            signer.params,
            proof_env,
            signer.avk.clone(),
            signer.total_stake,
        )
    }

    pub fn verify_sig(&self, sig: &StmSig<PE, H::F>, index: Index, msg: &[u8]) -> bool {
        let msgp = concat_avk_with_msg(&self.avk, msg);
        let ev = Msp::eval(&msgp, index, &sig.sigma);

        if !ev_lt_phi(self.params.phi_f, ev, sig.stake, self.total_stake)
            || !self
                .avk
                .check(&MTValue(sig.pk.mvk, sig.stake), sig.party, &sig.path)
        {
            return false;
        }
        Msp::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate<Proof>(
        &self,
        sigs: &[StmSig<PE, H::F>],
        indices: &[Index],
        msg: &[u8],
    ) -> Result<StmMultiSig<PE, Proof>, AggregationFailure>
    where
        Proof: MithrilProof<PE, H, E>,
    {
        let msgp = concat_avk_with_msg(&self.avk, msg);
        let mut evals = Vec::new();
        let mut mvks = Vec::new();
        let mut sigmas = Vec::new();
        let mut sigs_to_verify = Vec::new();
        let mut indices_to_verify = Vec::new();

        for (ix, sig) in dedup_sigs_for_indices::<H, PE>(sigs, indices) {
            if !self.verify_sig(sig, *ix, msg) {
                return Err(AggregationFailure::VerifyFailed);
            }

            evals.push(Msp::eval(&msgp, *ix, &sig.sigma));
            sigmas.push(sig.sigma);
            mvks.push(sig.pk.mvk);
            sigs_to_verify.push(sig.clone());
            indices_to_verify.push(*ix);

            if indices_to_verify.len() == self.params.k as usize {
                break;
            }
        }
        let n_unique = indices_to_verify.len();
        if n_unique < self.params.k as usize {
            return Err(AggregationFailure::NotEnoughSignatures(n_unique));
        }

        let ivk = Msp::aggregate_keys(&mvks);
        let mu = Msp::aggregate_sigs(msg, &sigmas);

        let statement = Statement {
            avk: self.avk.clone(),
            ivk: Rc::from(ivk),
            mu: Rc::from(mu),
            msg: Rc::from(msg),
            params: Rc::from(self.params),
            total_stake: self.total_stake,
        };
        let witness = Witness {
            sigs: sigs_to_verify,
            indices: indices_to_verify,
            evals,
        };
        // We're honest, so proving shouldn't fail
        let proof = Proof::prove(
            &self.proof_env,
            &self.proof_key,
            &Proof::RELATION,
            &Proof::S::from(statement),
            Proof::W::from(witness),
        )
        .expect("Constructed invalid proof");

        Ok(StmMultiSig { ivk, mu, proof })
    }

    pub fn verify_msig<Proof>(&self, msig: &StmMultiSig<PE, Proof>, msg: &[u8]) -> bool
    where
        Proof: MithrilProof<PE, H, E>,
    {
        let statement = Statement {
            // Specific to the message and signatures
            ivk: Rc::from(msig.ivk),
            mu: Rc::from(msig.mu),
            msg: Rc::from(msg),
            // These are "background" information"
            avk: self.avk.clone(),
            params: Rc::from(self.params),
            total_stake: self.total_stake,
        };
        if !msig.proof.verify(
            &self.proof_env,
            &self.verif_key,
            &Proof::RELATION,
            &Proof::S::from(statement),
        ) {
            return false;
        }
        let msgp = concat_avk_with_msg(&self.avk, msg);
        Msp::aggregate_ver(&msgp, &msig.ivk, &msig.mu)
    }
}

fn dedup_sigs_for_indices<'a, H: MTHashLeaf<MTValue<PE>>, PE: PairingEngine>(
    sigs: &'a [StmSig<PE, H::F>],
    indices: &'a [Index],
) -> impl IntoIterator<Item = (&'a Index, &'a StmSig<PE, H::F>)>
where
    PE::G1Projective: ToConstraintField<PE::Fq>,
{
    let mut sigs_by_index: HashMap<&Index, &StmSig<PE, H::F>> = HashMap::new();
    for (ix, sig) in indices.iter().zip(sigs) {
        if let Some(old_sig) = sigs_by_index.get(ix) {
            if sig.sigma < old_sig.sigma {
                sigs_by_index.insert(ix, sig);
            }
        } else {
            sigs_by_index.insert(ix, sig);
        }
    }

    sigs_by_index.into_iter()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mithril_proof::concat_proofs::*;
    use crate::proof::trivial::{TrivialEnv, TrivialProof};
    use ark_bls12_377::{Bls12_377, G1Projective as G1P, G2Projective as G2P};
    use ark_ec::ProjectiveCurve;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use proptest::test_runner::{RngAlgorithm::ChaCha, TestRng};
    use rand::{Rng, SeedableRng};
    use rayon::prelude::*;
    use std::collections::{HashMap, HashSet};

    type Proof = TrivialProof<Witness<Bls12_377, H>>;
    type Sig = StmMultiSig<Bls12_377, Proof>;
    type H = sha3::Sha3_256;
    type F = <H as MTHashLeaf<MTValue<Bls12_377>>>::F;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSigner<H, Bls12_377>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSigner<H, Bls12_377>> {
        let parties = stake.into_iter().enumerate().collect::<Vec<_>>();
        let mut kr = KeyReg::new(&parties);
        let mut trng = TestRng::deterministic_rng(ChaCha);
        let mut rng = rand_chacha::ChaCha8Rng::from_seed(trng.gen());
        let ps = parties
            .into_iter()
            .map(|(pid, stake)| {
                let mut p = StmInitializer::setup(params, pid, stake);
                p.register(&mut rng, &mut kr);
                p
            })
            .collect::<Vec<_>>();
        ps.into_iter()
            .map(|mut p| {
                p.retrieve_all(&kr);
                p.finish()
            })
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

    fn arb_num_parties_and_index(min: usize, max: usize) -> impl Strategy<Value = (usize, usize)> {
        (min..max).prop_flat_map(|n| (Just(n), 0..n))
    }

    fn find_signatures(
        m: u64,
        k: u64,
        msg: &[u8],
        ps: &[StmSigner<H, Bls12_377>],
        is: &[usize],
    ) -> (Vec<Index>, Vec<StmSig<Bls12_377, F>>) {
        let indices: Vec<_> = (1..m).collect();
        let res = indices
            .par_iter()
            .flat_map(|ix| {
                let mut ixs = Vec::new();
                let mut sigs = Vec::new();
                for i in is {
                    if let Some(sig) = ps[*i].sign(msg, *ix) {
                        sigs.push(sig);
                        ixs.push(*ix);
                    }
                }
                (ixs, sigs)
            })
            .unzip();

        res
    }

    proptest! {
        #[test]
        /// Test that when a party creates a signature it can be verified
        fn test_sig(msg in any::<[u8;16]>()) {
            let nparties = 2;
            let params = StmParameters { m: (nparties as u64), k: 1, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let p = &ps[0];
            let clerk = StmClerk::from_signer(p, TrivialEnv);


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
        fn test_aggregate_sig(nparties in 2_usize..30,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m, k, phi_f: 0.2 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0], TrivialEnv);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let (ixs, sigs) = find_signatures(m, k, &msg, &ps, &all_ps);

            let msig = clerk.aggregate::<ConcatProof<Bls12_377,H>>(&sigs, &ixs, &msg);

            match msig {
                Ok(aggr) =>
                    assert!(clerk.verify_msig::<ConcatProof<Bls12_377,H>>(&aggr, &msg)),
                Err(AggregationFailure::NotEnoughSignatures(n)) =>
                    assert!(n < params.k as usize),
                _ => unreachable!()
            }
        }
    }

    /// Pick N between min and max, and then
    /// generate a vector of N stakes summing to N*tstake,
    /// plus a subset S of 0..N such that the sum of the stakes at indices
    /// in S is astake*N
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

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]

        #[test]
        /// Test that when the adversaries do not hold sufficient stake, they can not form a quorum
        fn test_adversary_quorum(
            (adversaries, parties) in arb_parties_adversary_stake(8, 30, 16, 4),
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
            let ps = setup_parties(params, parties);

            let (ixs, sigs) = find_signatures(params.m,
                                              params.k,
                                              &msg,
                                              &ps,
                                              &adversaries.into_iter().collect::<Vec<_>>());

            assert!(sigs.len() < params.k as usize);

            let clerk = StmClerk::from_signer(&ps[0], TrivialEnv);

            let msig = clerk.aggregate::<ConcatProof<Bls12_377,H>>(&sigs, &ixs, &msg);
            match msig {
                Err(AggregationFailure::NotEnoughSignatures(n)) =>
                    assert!(n < params.k as usize),
                _ =>
                    unreachable!(),
            }
        }
    }

    #[derive(Debug)]
    struct ProofTest {
        n: usize,
        msig: Result<Sig, AggregationFailure>,
        clerk: StmClerk<H, Bls12_377, TrivialEnv>,
        msg: [u8; 16],
    }

    /// Run the protocol up to aggregation. This will produce a valid aggregation of signatures.
    /// The following tests mutate this aggregation so that the proof is no longer valid.
    fn arb_proof_setup(max_parties: usize) -> impl Strategy<Value = ProofTest> {
        any::<[u8; 16]>().prop_flat_map(move |msg| {
            (2..max_parties).prop_map(move |n| {
                let params = StmParameters {
                    m: 100,
                    k: 5,
                    phi_f: 0.2,
                };
                let ps = setup_equal_parties(params, n);
                let clerk = StmClerk::from_signer(&ps[0], TrivialEnv);

                let all_ps: Vec<usize> = (0..n).collect();
                let (ixs, sigs) = find_signatures(params.m, params.k, &msg, &ps, &all_ps);

                let msig = clerk.aggregate::<ConcatProof<Bls12_377, H>>(&sigs, &ixs, &msg);
                ProofTest {
                    n,
                    clerk,
                    msig,
                    msg,
                }
            })
        })
    }

    fn with_proof_mod<F>(mut tc: ProofTest, f: F)
    where
        F: Fn(&mut Sig, &mut StmClerk<H, Bls12_377, TrivialEnv>, &mut [u8; 16]),
    {
        match tc.msig {
            Ok(mut aggr) => {
                f(&mut aggr, &mut tc.clerk, &mut tc.msg);
                assert!(!tc.clerk.verify_msig(&aggr, &tc.msg))
            }
            _ => unreachable!(),
        }
    }

    proptest! {
        // Each of the tests below corresponds to falsifying a conjunct in the
        // defintion of the proved relation between statement & witness as
        // defined in the Mithril protocol
        #[test]
        fn test_invalid_proof_quorum(tc in arb_proof_setup(10),
                                     rnd in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                clerk.params.k += 1;
            })
        }
        #[test]
        fn test_invalid_proof_ivk(tc in arb_proof_setup(10),
                                  rnd in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let x = G2P::prime_subgroup_generator().mul(&[rnd]);
                aggr.ivk = MspMvk(x)
            })
        }
        #[test]
        fn test_invalid_proof_mu(tc in arb_proof_setup(10),
                                 rnd in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let x = G1P::prime_subgroup_generator().mul(&[rnd]);
                aggr.mu = MspSig(x)
            })
        }
        #[test]
        fn test_invalid_proof_index_bound(tc in arb_proof_setup(10),
                                          rnd in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                clerk.params.m = 1;
            })
        }
        #[test]
        fn test_invalid_proof_index_unique(tc in arb_proof_setup(10)) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                for i in aggr.proof.0.indices.iter_mut() {
                    *i %= clerk.params.k - 1
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10), i in any::<usize>()) {
            let n = tc.n;
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.0.sigs[pi].party = (aggr.proof.0.sigs[pi].party + 1) % n;
            })
        }
        #[test]
        fn test_invalid_proof_eval(tc in arb_proof_setup(10), i in any::<usize>(), v in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.0.evals[pi] = v;
            })
        }
        #[test]
        fn test_invalid_proof_stake(tc in arb_proof_setup(10), i in any::<usize>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.0.evals[pi] = 0;
            })
        }
    }
}
