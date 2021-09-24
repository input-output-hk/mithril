//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.

use super::{ev_lt_phi, Index, PartyId, Path, Stake};
use crate::key_reg::KeyReg;
use crate::mithril_hash::{IntoHash, MithrilHasher};
use crate::merkle_tree::{MerkleTree};
use crate::mithril_field::{
    HashToCurve,
    wrapper::{
        MithrilField,
        MithrilFieldWrapper
    },
};
use crate::msp::{Msp, MspMvk, MspPk, MspSig, MspSk};
use crate::proof::Proof;

use ark_ec::{
    PairingEngine,
};
use std::collections::HashMap;

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
pub struct StmInitializer<P>
where
    P: PairingEngine,
    P::Fr: MithrilField,
{
    party_id: PartyId,
    stake: Stake,
    avk: Option<MerkleTree<P::Fr>>,
    sk: Option<MspSk<P>>,
    pk: Option<MspPk<P>>,
    total_stake: Option<Stake>,
    params: StmParameters,
}

/// Participant in the protocol. Can sign messages.
pub struct StmSigner<PE>
where
    PE: PairingEngine,
{
    party_id: PartyId,
    stake: Stake,
    params: StmParameters,
    total_stake: Stake,
    avk: MerkleTree<PE::Fr>,
    sk: MspSk<PE>,
    pk: MspPk<PE>,
}

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
pub struct StmClerk<PE>
where
    PE: PairingEngine
{
    avk: MerkleTree<PE::Fr>,
    params: StmParameters,
    total_stake: Stake,
}

/// Signature created by a single party who has won the lottery.
#[derive(Clone, Debug)]
pub struct StmSig<P: PairingEngine> {
    pub sigma: MspSig<P>,
    pub pk: MspPk<P>,
    pub party: PartyId,
    pub stake: Stake,
    pub path: Path<P::Fr>,
}

/// Aggregated signature of many parties.
/// Contains proof that it is well-formed.
#[derive(Clone)]
pub struct StmMultiSig<P,E>
where
    E: PairingEngine
{
    ivk: MspMvk<E>,
    mu: MspSig<E>,
    proof: P,
}

/// Error types for aggregation.
#[derive(Debug)]
pub enum AggregationFailure {
    NotEnoughSignatures(usize),
    /// How many signatures we got
    VerifyFailed,
}

impl<P> StmInitializer<P>
where
    P: PairingEngine,
    P::G1Affine: HashToCurve,
    P::G2Projective: IntoHash<P::Fr>,
    P::Fr: MithrilField,
    P::Fqe: IntoHash<P::Fr>,
{
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

    pub fn register(&mut self, kr: &mut KeyReg<P>) {
        // (msk_i, mvk_i, k_i) <- MSP.Gen(Param)
        // (vk_i, sk_i) := ((mvk_i, k_i), msk_i)
        // send (Register, sid, vk_i) to F_KR
        let (sk, pk) = Msp::gen();
        self.sk = Some(sk);
        self.pk = Some(pk.clone());
        kr.register(self.party_id, self.stake, pk);
    }

    pub fn retrieve_all(&mut self, kr: &KeyReg<P>) {
        // Reg := (K(P_i), stake_i)
        // Reg is padded to length N using null entries of stake 0
        // AVK <- MT.Create(Reg)
        let reg = kr.retrieve_all();
        self.avk = Some(MerkleTree::create(&reg));
        // get total stake
        self.total_stake = Some(reg.iter().filter_map(|p| p.map(|(_, s)| s)).sum());
    }

    pub fn finish(self) -> StmSigner<P> {
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

impl<PE> StmSigner<PE>
where
    PE: PairingEngine,
    PE::G1Affine: HashToCurve,
{
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

    pub fn sign(&self, msg: &[u8], index: Index) -> Option<StmSig<PE>> {
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

impl<PE> StmClerk<PE>
where
    PE: PairingEngine,
    PE::G1Affine: HashToCurve,
    PE::G2Projective: IntoHash<PE::Fr>,
    PE::Fr: MithrilField,
    MspSig<PE>: Ord
{
    pub fn new(params: StmParameters, avk: MerkleTree<PE::Fr>, total_stake: Stake) -> Self {
        Self {
            params,
            avk,
            total_stake,
        }
    }

    pub fn from_signer(signer: &StmSigner<PE>) -> Self {
        Self::new(signer.params, signer.avk.clone(), signer.total_stake)
    }

    pub fn verify_sig(&self, sig: &StmSig<PE>, index: Index, msg: &[u8]) -> bool {
        let msgp = self.avk.concat_with_msg(msg);
        let ev = Msp::eval(&msgp, index, &sig.sigma);
        if !ev_lt_phi(self.params.phi_f, ev, sig.stake, self.total_stake)
            || !self
                .avk
                .check(&(sig.pk.clone(), sig.stake), sig.party, &sig.path)
        {
            return false;
        }
        Msp::ver(&msgp, &sig.pk.mvk, &sig.sigma)
    }

    pub fn aggregate<P: Proof<PE>>(
        &self,
        sigs: &[StmSig<PE>],
        indices: &[Index],
        msg: &[u8],
    ) -> Result<StmMultiSig<P,PE>, AggregationFailure> {
        let msgp = self.avk.concat_with_msg(msg);
        let mut evals = Vec::new();
        let mut mvks = Vec::new();
        let mut sigmas = Vec::new();
        let mut sigs_to_verify = Vec::new();
        let mut indices_to_verify = Vec::new();

        for (ix, sig) in dedup_sigs_for_indices(sigs, indices) {
            if !self.verify_sig(&sig, *ix, msg) {
                return Err(AggregationFailure::VerifyFailed);
            }
            evals.push(Msp::eval(&msgp, *ix, &sig.sigma));
            sigmas.push(sig.sigma);
            mvks.push(sig.pk.mvk);
            sigs_to_verify.push(sig.clone());
            indices_to_verify.push(*ix);
        }
        let n_unique = indices_to_verify.len();
        if n_unique < self.params.k as usize {
            return Err(AggregationFailure::NotEnoughSignatures(n_unique));
        }

        let ivk = Msp::aggregate_keys(&mvks);
        let mu = Msp::aggregate_sigs(msg, &sigmas);

        let proof = P::prove(
            &self.avk,
            &ivk,
            msg,
            &sigs_to_verify,
            &indices_to_verify,
            &evals,
        );
        Ok(StmMultiSig { ivk, mu, proof })
    }

    pub fn verify_msig<P: Proof<PE>>(&self, msig: &StmMultiSig<P, PE>, msg: &[u8]) -> bool {
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

impl<P: PairingEngine> IntoHash<P::Fr> for MspMvk<P>
where
    P::G2Projective: IntoHash<P::Fr>,
    P::Fr: MithrilField,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, P::Fr>) -> MithrilFieldWrapper<P::Fr> {
        self.0.into_hash(hasher)
    }
}

impl<P: PairingEngine> IntoHash<P::Fr> for MspPk<P>
where
    P::G2Projective: IntoHash<P::Fr>,
    P::Fr: MithrilField,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, P::Fr>) -> MithrilFieldWrapper<P::Fr> {
        self.mvk.into_hash(hasher)
    }
}

fn dedup_sigs_for_indices<'a, P:PairingEngine>(
    sigs: &'a [StmSig<P>],
    indices: &'a [Index],
) -> impl IntoIterator<Item = (&'a Index, &'a StmSig<P>)>
where
    MspSig<P>: Ord
{
    let mut sigs_by_index: HashMap<&Index, &StmSig<P>> = HashMap::new();
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
    use crate::proof::ConcatProof;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use rayon::prelude::*;
    use std::collections::{HashMap, HashSet};
    use ark_bls12_377::Bls12_377;

    fn setup_equal_parties(params: StmParameters, nparties: usize) -> Vec<StmSigner<Bls12_377>> {
        let stake = vec![1; nparties];
        setup_parties(params, stake)
    }

    fn setup_parties(params: StmParameters, stake: Vec<Stake>) -> Vec<StmSigner<Bls12_377>> {
        let mut kr = KeyReg::new();
        let ps = stake
            .iter()
            .enumerate()
            .map(|(pid, stake)| {
                let mut p = StmInitializer::setup(params, pid, *stake);
                p.register(&mut kr);
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
        ps: &mut [StmSigner<Bls12_377>],
        is: &[usize],
    ) -> (Vec<Index>, Vec<StmSig<Bls12_377>>) {
        let indices: Vec<_> = (1..m).collect();
        let res = indices
            .par_iter()
            .flat_map(|ix| {
                let mut ixs = Vec::new();
                let mut sigs = Vec::new();
                for i in is {
                    if let Some(sig) = ps[*i].sign(&msg, *ix) {
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
            let mut ps = setup_equal_parties(params, nparties);
            let p = &mut ps[0];
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
        fn test_aggregate_sig(nparties in 2_usize..16,
                              m in 10_u64..20,
                              k in 1_u64..5,
                              msg in any::<[u8;16]>()) {
            let params = StmParameters { m: m, k: k, phi_f: 0.2 };
            let mut ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0]);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let (ixs, sigs) = find_signatures(m, k, &msg, &mut ps, &all_ps);

            let msig = clerk.aggregate::<ConcatProof<Bls12_377>>(&sigs, &ixs, &msg);

            match msig {
                Ok(aggr) =>
                    assert!(clerk.verify_msig(&aggr, &msg)),
                Err(AggregationFailure::NotEnoughSignatures(n)) =>
                    assert!(n < params.k as usize),
                _ =>
                    assert!(false)
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
            (adversaries, parties) in arb_parties_adversary_stake(2, 10, 16, 4),
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

            let clerk = StmClerk::from_signer(&ps[0]);

            let msig = clerk.aggregate::<ConcatProof<Bls12_377>>(&sigs, &ixs, &msg);
            match msig {
                Err(AggregationFailure::NotEnoughSignatures(n)) =>
                    assert!(n < params.k as usize),
                _ =>
                    assert!(false),
            }
        }
    }
}
