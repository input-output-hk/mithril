//! Top-level API for Mithril Stake-based Threshold Multisignature scheme.

use super::{concat_avk_with_msg, ev_lt_phi, Index, PartyId, Path, Stake};
use crate::key_reg::RegParty;
use crate::merkle_tree::{MTHashLeaf, MerkleTree};
use crate::mithril_proof::{MithrilProof, MithrilStatement, MithrilWitness};
use crate::msp::{Msp, MspMvk, MspPk, MspSig, MspSk};
use crate::proof::ProverEnv;
use ark_ec::PairingEngine;
use ark_ff::bytes::{FromBytes, ToBytes};
use ark_ff::ToConstraintField;
use ark_std::io::{Read, Write};
use rand::Rng;
use std::collections::HashMap;
use std::convert::From;
use std::convert::TryInto;
use std::rc::Rc;

/// The values that are represented in the Merkle Tree.
#[derive(Debug, Clone, Copy)]
pub struct MTValue<PE: PairingEngine>(pub MspMvk<PE>, pub Stake);

/// Used to set protocol parameters.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
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
pub struct StmInitializer<PE>
where
    PE: PairingEngine,
{
    /// This participant's Id
    party_id: PartyId,
    /// This participant's stake
    stake: Stake,
    /// Current protocol instantiation parameters
    params: StmParameters,
    /// Secret key
    sk: MspSk<PE>,
    /// Verification (public) key + proof of possession
    pk: MspPk<PE>,
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
    Proof: MithrilProof,
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

impl<PE: PairingEngine, F: FromBytes> FromBytes for StmSig<PE, F> {
    fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let sigma = MspSig::<PE>::read(&mut reader)?;
        let pk = MspPk::<PE>::read(&mut reader)?;
        let party_u64 = u64::read(&mut reader)?;
        let party = party_u64 as usize;
        let stake = Stake::read(&mut reader)?;
        let path = Path::read(&mut reader)?;

        Ok(StmSig {
            sigma,
            pk,
            party,
            stake,
            path,
        })
    }
}

impl<PE: PairingEngine, F: ToBytes> ToBytes for StmSig<PE, F> {
    fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
        self.sigma.write(&mut writer)?;
        self.pk.write(&mut writer)?;
        let party: u64 = self.party.try_into().unwrap();
        party.write(&mut writer)?;
        self.stake.write(&mut writer)?;
        self.path.write(&mut writer)?;

        Ok(())
    }
}

impl<PE: PairingEngine, Proof: MithrilProof> FromBytes for StmMultiSig<PE, Proof> {
    fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let ivk = MspMvk::read(&mut reader)?;
        let mu = MspSig::read(&mut reader)?;
        let proof = Proof::read(&mut reader)?;

        Ok(StmMultiSig { ivk, mu, proof })
    }
}
impl<PE: PairingEngine, Proof: MithrilProof> ToBytes for StmMultiSig<PE, Proof> {
    fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
        self.ivk.write(&mut writer)?;
        self.mu.write(&mut writer)?;
        self.proof.write(&mut writer)
    }
}

impl<PE> StmInitializer<PE>
where
    PE: PairingEngine,
    MspPk<PE>: std::hash::Hash,
{
    //////////////////////////
    // Initialization phase //
    //////////////////////////

    /// Builds an `StmInitializer` that is ready to register with the key registration service
    pub fn setup<R>(params: StmParameters, party_id: PartyId, stake: Stake, rng: &mut R) -> Self
    where
        R: Rng + rand::CryptoRng + ?Sized,
    {
        let (sk, pk) = Msp::gen(rng);
        Self {
            party_id,
            stake,
            pk,
            sk,
            params,
        }
    }

    pub fn generate_new_key<R>(&mut self, rng: &mut R)
    where
        R: Rng + rand::CryptoRng + ?Sized,
    {
        let (sk, pk) = Msp::gen(rng);
        self.sk = sk;
        self.pk = pk;
    }

    pub fn secret_key(&self) -> MspSk<PE> {
        self.sk
    }

    pub fn set_secret_key(&mut self, sk: MspSk<PE>) {
        self.sk = sk;
    }

    pub fn verification_key(&self) -> MspPk<PE> {
        self.pk
    }

    pub fn set_verification_key(&mut self, pk: MspPk<PE>) {
        self.pk = pk;
    }

    pub fn set_stake(&mut self, stake: Stake) {
        self.stake = stake;
    }

    pub fn stake(&self) -> Stake {
        self.stake
    }

    pub fn party_id(&self) -> PartyId {
        self.party_id
    }

    pub fn set_params(&mut self, params: StmParameters) {
        self.params = params;
    }

    /// Build the avk for the given list of parties.
    ///
    /// Note that if this StmInitializer was modified *between* the last call to `register`,
    /// then the resulting `StmSigner` may not be able to produce valid signatures.
    ///
    /// Returns an StmSigner specialized to
    /// (1) this StmSigner's ID and current stake
    /// (2) this StmSigner's parameter valuation
    /// (3) the avk as built from the current registered parties (according to the registration service)
    /// (4) the current total stake (according to the registration service)
    pub fn new_signer<H>(&self, reg: &[RegParty<PE>]) -> StmSigner<H, PE>
    where
        H: MTHashLeaf<MTValue<PE>>,
    {
        // The paper uses Reg as the vector of values with which to initialize
        // the merkle tree. The implementation stores MTValues (derived from
        // this vector) in the tree.
        let mtvals: Vec<MTValue<PE>> = reg.iter().map(|rp| MTValue(rp.pk.mvk, rp.stake)).collect();
        // AVK <- MT.Create(Reg)
        let avk = MerkleTree::create(&mtvals);
        let my_index = reg
            .iter()
            .enumerate()
            .find(|(i, rp)| rp.party_id == self.party_id)
            .unwrap_or_else(|| panic!("party unknown: {}", self.party_id))
            .0;
        let total_stake = mtvals.iter().map(|s| s.1).sum();

        StmSigner {
            party_id: self.party_id,
            avk_idx: my_index,
            stake: self.stake,
            params: self.params,
            avk,
            sk: self.sk,
            pk: self.pk,
            total_stake,
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

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// The `From` bound on `Proof::Statement` allows `aggregate` to translate
    /// from the `Mithril` specific statement and witness types to their proof system-specific
    /// representations.
    pub fn aggregate<Proof>(
        &self,
        sigs: &[StmSig<PE, H::F>],
        indices: &[Index],
        msg: &[u8],
    ) -> Result<StmMultiSig<PE, Proof>, AggregationFailure>
    where
        Proof: MithrilProof<Env = E>,
        Proof::Statement: From<MithrilStatement<PE, H>>,
        Proof::Witness: From<MithrilWitness<PE, H>>,
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
        let mu = Msp::aggregate_sigs(&sigmas);

        let statement = MithrilStatement {
            avk: self.avk.clone(),
            ivk,
            mu,
            msg: msg.to_vec(),
            params: self.params,
            total_stake: self.total_stake,
        };
        let witness = MithrilWitness {
            sigs: sigs_to_verify,
            indices: indices_to_verify,
            evals,
        };
        // We're honest, so proving shouldn't fail
        let proof = Proof::prove(
            &self.proof_env,
            &self.proof_key,
            &Proof::RELATION,
            &Proof::Statement::from(statement),
            Proof::Witness::from(witness),
        )
        .expect("Constructed invalid proof");

        Ok(StmMultiSig { ivk, mu, proof })
    }

    /// Verify an aggregation of signatures.
    ///
    /// The `From` bound on `Proof::Statement` allows `aggregate` to translate
    /// from the `Mithril` specific statement and witness types to their proof system-specific
    /// representations.
    pub fn verify_msig<Proof>(&self, msig: &StmMultiSig<PE, Proof>, msg: &[u8]) -> bool
    where
        Proof: MithrilProof<Env = E>,
        Proof::Statement: From<MithrilStatement<PE, H>>,
        Proof::Witness: From<MithrilWitness<PE, H>>,
    {
        let statement = MithrilStatement {
            // Specific to the message and signatures
            ivk: msig.ivk,
            mu: msig.mu,
            msg: msg.to_vec(),
            // These are "background" information"
            avk: self.avk.clone(),
            params: self.params,
            total_stake: self.total_stake,
        };
        if !msig.proof.verify(
            &self.proof_env,
            &self.verif_key,
            &Proof::RELATION,
            &Proof::Statement::from(statement),
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
    use crate::key_reg::*;
    use crate::mithril_proof::concat_proofs::*;
    use crate::proof::trivial::TrivialEnv;
    use ark_bls12_377::{Bls12_377, G1Projective as G1P, G2Projective as G2P};
    use ark_ec::ProjectiveCurve;
    use proptest::collection::{hash_map, vec};
    use proptest::prelude::*;
    use proptest::test_runner::{RngAlgorithm::ChaCha, TestRng};
    use rand::{Rng, SeedableRng};
    use rayon::prelude::*;
    use std::collections::{HashMap, HashSet};

    type Proof = ConcatProof<Bls12_377, H>;
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

        // The needless_collect lint is not correct here
        #[allow(clippy::needless_collect)]
        let ps = parties
            .into_iter()
            .map(|(pid, stake)| {
                let p = StmInitializer::setup(params, pid, stake, &mut rng);
                kr.register(p.party_id(), p.stake(), p.verification_key())
                    .unwrap();
                p
            })
            .collect::<Vec<_>>();
        let reg = kr.retrieve_all();
        ps.into_iter().map(|p| p.new_signer(&reg)).collect()
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

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(10))]
        #[test]
        fn test_sig_serialize_deserialize(nparties in 2_usize..10,
                                          msg in any::<[u8;16]>()) {
            let params = StmParameters { m: 10, k: 1, phi_f: 1.0 };
            let ps = setup_equal_parties(params, nparties);
            let clerk = StmClerk::from_signer(&ps[0], TrivialEnv);

            let all_ps: Vec<usize> = (0..nparties).collect();
            let (ixs, sigs) = find_signatures(10, 1, &msg, &ps, &all_ps);
            let msig = clerk.aggregate::<ConcatProof<Bls12_377,H>>(&sigs, &ixs, &msg);
            match msig {
                Ok(aggr) => {
                    let bytes: Vec<u8> = ark_ff::to_bytes!(aggr).unwrap();
                    let aggr2 = StmMultiSig::read(&bytes[..]).unwrap();
                    assert!(clerk.verify_msig::<ConcatProof<Bls12_377,H>>(&aggr2, &msg));
                },
                _ => ()
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
                    msig,
                    clerk,
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
                for i in aggr.proof.witness.indices.iter_mut() {
                    *i %= clerk.params.k - 1
                }
            })
        }
        #[test]
        fn test_invalid_proof_path(tc in arb_proof_setup(10), i in any::<usize>()) {
            let n = tc.n;
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.witness.sigs[pi].party = (aggr.proof.witness.sigs[pi].party + 1) % n;
            })
        }
        #[test]
        fn test_invalid_proof_eval(tc in arb_proof_setup(10), i in any::<usize>(), v in any::<u64>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.witness.evals[pi] = v;
            })
        }
        #[test]
        fn test_invalid_proof_stake(tc in arb_proof_setup(10), i in any::<usize>()) {
            with_proof_mod(tc, |aggr, clerk, msg| {
                let pi = i % clerk.params.k as usize;
                aggr.proof.witness.evals[pi] = 0;
            })
        }
    }
}
