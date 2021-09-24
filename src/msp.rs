//! Base multisignature scheme. Currently using BLS12.

use super::mithril_curves::{
    hash_to_curve,
    wrapper::{MithrilField, MithrilFieldWrapper},
    AsCoord,
};
use super::mithril_hash::{IntoHash, MithrilHasher};
use super::Index;

use ark_ec::{AffineCurve, PairingEngine};
use ark_ff::bytes::ToBytes;
use blake2::VarBlake2b;
use digest::{Update, VariableOutput};
use rand_core::{OsRng, RngCore};
use std::cmp::Ordering;
use std::marker::PhantomData;

pub struct Msp<P: PairingEngine> {
    x: PhantomData<P>,
}

#[derive(Clone, Copy)]
pub struct MspSk<P: PairingEngine>(P::Fr);

#[derive(Debug, Clone, Copy)]
pub struct MspMvk<P: PairingEngine>(pub P::G2Projective);

#[derive(Debug, Clone, Copy)]
pub struct MspPk<P: PairingEngine> {
    pub mvk: MspMvk<P>,
    pub k1: P::G1Projective,
    pub k2: P::G1Projective,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MspSig<P: PairingEngine>(P::G1Projective);

impl<P: PairingEngine> MspSig<P>
where
    P::G1Projective: AsCoord,
{
    fn cmp_msp_sig(&self, other: &Self) -> Ordering {
        self.0.as_coords().cmp(&other.0.as_coords())
    }
}

impl<P: PairingEngine> PartialOrd for MspSig<P>
where
    P::G1Projective: AsCoord,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_sig(other))
    }
}

impl<P: PairingEngine> Ord for MspSig<P>
where
    P::G1Projective: AsCoord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_sig(other)
    }
}

static POP: &[u8] = b"PoP";
static M: &[u8] = b"M";

impl<P> Msp<P>
where
    P: PairingEngine,
{
    pub fn gen() -> (MspSk<P>, MspPk<P>) {
        // sk=x <- Zq
        let mut rng = OsRng::default();
        let x = P::Fr::from(rng.next_u64());
        // mvk <- g2^x
        let mvk = MspMvk(P::G2Affine::prime_subgroup_generator().mul(x));
        // k1 <- H_G1("PoP"||mvk)^x
        let k1 = hash_to_curve::<P::G1Affine>([POP, &mvk.to_bytes()].concat().as_ref()).mul(x);
        // k2 <- g1^x
        let k2 = P::G1Affine::prime_subgroup_generator().mul(x);
        // return sk,mvk,k=(k1,k2)
        (MspSk(x), MspPk { mvk, k1, k2 })
    }

    pub fn check(pk: &MspPk<P>) -> bool {
        // if e(k1,g2) = e(H_G1("PoP"||mvk),mvk)
        //      and e(g1,mvk) = e(k2,g2)
        //      are both true, return 1
        let mvk_g2 = P::G2Affine::from(pk.mvk.0);
        let e_k1_g2 = P::pairing(pk.k1.into(), P::G2Affine::prime_subgroup_generator());
        let h_pop_mvk = hash_to_curve::<P::G1Affine>([POP, &pk.mvk.to_bytes()].concat().as_ref());
        let e_hg1_mvk = P::pairing(h_pop_mvk, mvk_g2);

        let e_g1_mvk = P::pairing(P::G1Affine::prime_subgroup_generator(), mvk_g2);
        let e_k2_g2 = P::pairing(pk.k2.into(), P::G2Affine::prime_subgroup_generator());

        (e_k1_g2 == e_hg1_mvk) && (e_g1_mvk == e_k2_g2)
    }

    pub fn sig(sk: &MspSk<P>, msg: &[u8]) -> MspSig<P> {
        // return sigma <- H_G1("M"||msg)^x
        let g1 = hash_to_curve::<P::G1Affine>([M, msg].concat().as_ref());
        MspSig(g1.mul(sk.0))
    }

    pub fn ver(msg: &[u8], mvk: &MspMvk<P>, sigma: &MspSig<P>) -> bool {
        // return 1 if e(sigma,g2) = e(H_G1("M"||msg),mvk)
        let e_sigma_g2 = P::pairing(
            P::G1Affine::from(sigma.0),
            P::G2Affine::prime_subgroup_generator(),
        );
        let g1 = hash_to_curve::<P::G1Affine>([M, msg].concat().as_ref());
        let e_hg1_mvk = P::pairing(g1, P::G2Affine::from(mvk.0));

        e_sigma_g2 == e_hg1_mvk
    }

    // MSP.AKey
    pub fn aggregate_keys(mvks: &[MspMvk<P>]) -> MspMvk<P> {
        MspMvk(mvks.iter().map(|s| s.0).sum())
    }

    // MSP.Aggr
    pub fn aggregate_sigs(msg: &[u8], sigmas: &[MspSig<P>]) -> MspSig<P> {
        // XXX: what is d?
        MspSig(sigmas.iter().map(|s| s.0).sum())
    }

    // MSP.AVer
    pub fn aggregate_ver(msg: &[u8], ivk: &MspMvk<P>, mu: &MspSig<P>) -> bool {
        Self::ver(msg, ivk, mu)
    }

    pub fn eval(msg: &[u8], index: Index, sigma: &MspSig<P>) -> u64 {
        let mut hasher: VarBlake2b = VariableOutput::new(8).unwrap();
        // // H("map"||msg||index||sigma)
        hasher.update(
            &[
                "map".as_bytes(),
                msg,
                &index.to_le_bytes(),
                &sigma.to_bytes(),
            ]
            .concat(),
        );
        let mut dest = [0 as u8; 8];
        hasher.finalize_variable(|out| {
            dest.copy_from_slice(out);
        });
        u64::from_le_bytes(dest)
        // // XXX: See section 6 to implement M from Elligator Squared
        // // return ev <- M_msg,index(sigma)
    }
}

impl<P: PairingEngine> MspMvk<P> {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![];
        self.0.write(&mut bytes).unwrap();
        bytes
    }
}

impl<P: PairingEngine> MspSig<P> {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![];
        self.0.write(&mut bytes).unwrap();
        bytes
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

#[cfg(test)]
mod tests {
    use super::*;
    use ark_bls12_377::{Bls12_377, Fr, G1Affine, G2Affine};
    use proptest::prelude::*;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn test_pair_prop(x in any::<u64>(), y in any::<u64>()) {
            // Sanity check that the library behaves as expected
            let sx = Fr::from(x);
            let sy = Fr::from(y);
            let gt = Bls12_377::pairing(G1Affine::prime_subgroup_generator().mul(sx),
                                        G2Affine::prime_subgroup_generator().mul(sy));
            let should_be = Bls12_377::pairing(G1Affine::prime_subgroup_generator().mul(sx * sy), G2Affine::prime_subgroup_generator());
            assert!(gt == should_be);
        }

        #[test]
        fn test_sig(msg in prop::collection::vec(any::<u8>(), 1..128)) {
            let (sk, pk) = Msp::<Bls12_377>::gen();
            let sig = Msp::sig(&sk, &msg);
            assert!(Msp::ver(&msg, &pk.mvk, &sig));
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                            r in any::<u64>()) {
            let (sk, pk) = Msp::<Bls12_377>::gen();
            let x = MspSig(G1Affine::prime_subgroup_generator().mul(Fr::from(r)));
            assert!(!Msp::ver(&msg, &pk.mvk, &x));
        }

        #[test]
        fn test_aggregate_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                              num_sigs in 1..16) {
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..num_sigs {
                let (sk, pk) = Msp::<Bls12_377>::gen();
                let sig = Msp::sig(&sk, &msg);
                assert!(Msp::ver(&msg, &pk.mvk, &sig));
                sigs.push(sig);
                mvks.push(pk.mvk);
            }
            let ivk = Msp::aggregate_keys(&mvks);
            let mu = Msp::aggregate_sigs(&msg, &sigs);
            assert!(Msp::aggregate_ver(&msg, &ivk, &mu));
        }

        #[test]
        fn test_eval_sanity_check(msg in prop::collection::vec(any::<u8>(), 1..128),
                                  idx in any::<u64>(),
                                  s in any::<u64>()) {
            let sigma = MspSig(G1Affine::prime_subgroup_generator().mul(Fr::from(s)));
            Msp::<Bls12_377>::eval(&msg, idx, &sigma);
        }
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = Msp::<Bls12_377>::gen();
            assert!(Msp::check(&pk));
        }
    }
}
