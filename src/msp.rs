//! Multisignature scheme API

use super::Index;

use blstrs::{pairing, Scalar, Field, G1Affine, G1Projective, G2Projective, G2Affine};
use rand_core::OsRng;
use groupy::CurveAffine;
use blake2::VarBlake2b;
use digest::{VariableOutput,Update};

pub struct Msp { }

#[derive(Clone,Copy)]
pub struct MspSk(Scalar);

#[derive(Debug,Clone,Copy)]
pub struct MspMvk(pub G2Projective);

#[derive(Debug,Clone,Copy)]
pub struct MspPk {
    pub mvk: MspMvk,
    pub k1: G1Projective,
    pub k2: G1Projective,
}

#[derive(Debug,Clone,Copy)]
pub struct MspSig(G1Projective);

static POP: &[u8] = b"PoP";
static M: &[u8]   = b"M";

impl Msp {
    pub fn gen() -> (MspSk, MspPk) {
        // sk=x <- Zq
        let mut rng = OsRng::default();
        let x = Scalar::random(&mut rng);
        // mvk <- g2^x
        let mvk = MspMvk(G2Affine::one() * x);
        // k1 <- H_G1("PoP"||mvk)^x
        let k1 = hash_to_g1(POP, &mvk.to_bytes()) * x;
        // k2 <- g1^x
        let k2 = G1Affine::one() * x;
        // return sk,mvk,k=(k1,k2)
        (MspSk(x), MspPk { mvk, k1, k2 })

    }

    pub fn check(pk: &MspPk) -> bool {
        // if e(k1,g2) = e(H_G1("PoP"||mvk),mvk)
        //      and e(g1,mvk) = e(k2,g2)
        //      are both true, return 1
        let mvk_g2 = G2Affine::from(pk.mvk.0);
        let e_k1_g2   = pairing(pk.k1.into(), G2Affine::one());
        let h_pop_mvk = hash_to_g1(POP, &pk.mvk.to_bytes());
        let e_hg1_mvk = pairing(h_pop_mvk, mvk_g2);

        let e_g1_mvk = pairing(G1Affine::one(), mvk_g2);
        let e_k2_g2  = pairing(pk.k2.into(), G2Affine::one());

        (e_k1_g2 == e_hg1_mvk) && (e_g1_mvk == e_k2_g2)
    }

    pub fn sig(sk: &MspSk, msg: &[u8]) -> MspSig {
        // return sigma <- H_G1("M"||msg)^x
        let g1 = hash_to_g1(M, msg);
        MspSig(g1 * sk.0)
    }

    pub fn ver(msg: &[u8], mvk: &MspMvk, sigma: &MspSig) -> bool {
        // return 1 if e(sigma,g2) = e(H_G1("M"||msg),mvk)
        let e_sigma_g2 = pairing(G1Affine::from(sigma.0), G2Affine::one());
        let e_hg1_mvk  = pairing(hash_to_g1(M, msg), G2Affine::from(mvk.0));

        e_sigma_g2 == e_hg1_mvk
    }

    // MSP.AKey
    pub fn aggregate_keys(mvks: &[MspMvk]) -> MspMvk {
        MspMvk(mvks
               .iter()
               .map(|s| s.0)
               .sum())
    }

    // MSP.Aggr
    pub fn aggregate_sigs(msg: &[u8], sigmas: &[MspSig]) -> MspSig {
        // XXX: what is d?
        MspSig(sigmas
               .iter()
               .map(|s| s.0)
               .sum())
    }

    // MSP.AVer
    pub fn aggregate_ver(msg: &[u8], ivk: &MspMvk, mu: &MspSig) -> bool {
        Self::ver(msg, ivk, mu)
    }

    pub fn eval(msg: &[u8], index: Index, sigma: &MspSig) -> u64 {
        let mut hasher : VarBlake2b = VariableOutput::new(8).unwrap();
        // H("map"||msg||index||sigma)
        hasher.update(&["map".as_bytes(),
                        msg,
                        &index.to_le_bytes(),
                        &sigma.0.to_uncompressed()].concat());
        let mut dest = [0 as u8; 8];
        hasher.finalize_variable(|out| {
            dest.copy_from_slice(out);
        });
        u64::from_le_bytes(dest)
        // XXX: See section 6 to implement M from Elligator Squared
        // return ev <- M_msg,index(sigma)
    }
}

impl MspMvk {
    pub fn to_bytes(&self) -> [u8; 96] {
        // Notes: to_vec() here causes a segfault later, why?
        self.0.to_uncompressed()
    }
}

fn hash_to_g1(tag: &[u8], bytes: &[u8]) -> G1Affine {
    // k1 <- H_G1("PoP"||mvk)^x
    G1Affine::from(G1Projective::hash_to_curve(bytes, b"mithril", tag))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_pair_prop(x in any::<u64>(),
                          y in any::<u64>()) {
            // Sanity check that the library behaves as expected
            let sx = blstrs::Scalar::from(x);
            let sy = blstrs::Scalar::from(y);
            let gt = pairing((G1Affine::one() * sx).into(),
                             (G2Affine::one() * sy).into());
            let should_be = pairing((G1Affine::one() * (sx * sy)).into(), G2Affine::one());
            assert!(gt == should_be);
        }

        #[test]
        fn test_sig(msg in prop::collection::vec(any::<u8>(), 1..128)) {
            let (sk, pk) = Msp::gen();
            let sig = Msp::sig(&sk, &msg);
            assert!(Msp::ver(&msg, &pk.mvk, &sig));
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                            r in prop::collection::vec(any::<u64>(), 100)) {
            let (sk, pk) = Msp::gen();

            // Low probability is still greater than zero :)
            let mut success = 0;
            for i in 1..100 {
                let x = MspSig(G1Affine::one() * Scalar::from(r[i]));
                if Msp::ver(&msg, &pk.mvk, &x) { success += 1; }
            }

            assert!(success <= 5);
        }

        #[test]
        fn test_aggregate_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                              num_sigs in 1..16) {
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..num_sigs {
                let (sk, pk) = Msp::gen();
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
            let sigma = MspSig(G1Affine::one() * blstrs::Scalar::from(s));
            Msp::eval(&msg, idx, &sigma);
        }
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = Msp::gen();
            assert!(Msp::check(&pk));
        }
    }
}
