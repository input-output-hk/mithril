//! Multisignature scheme API

use super::{Unknown, Index};

use blstrs::{pairing, Scalar, Field, G1Affine, G1Projective, G2Projective, G2Affine};
use rand_core::OsRng;
use groupy::CurveAffine;

pub struct MSP { }

#[derive(Clone,Copy)]
pub struct SK(Scalar);

#[derive(Clone,Copy)]
pub struct MVK(G2Projective);

#[derive(Clone,Copy)]
pub struct PK {
    pub mvk: MVK,
    pub k1: G1Projective,
    pub k2: G1Projective,
}

#[derive(Clone,Copy)]
pub struct Sig(G1Projective);

fn hash_to_g1(bytes: &[u8]) -> G1Affine {
    // k1 <- H_G1("PoP"||mvk)^x
    G1Affine::from(G1Projective::hash_to_curve(bytes, "mithril".as_bytes(), &[]))
}

static POP: &[u8] = "PoP".as_bytes();
static M: &[u8]   = "M".as_bytes();

impl MSP {
    pub fn gen() -> (SK, PK) {
        // sk=x <- Zq
        let mut rng = OsRng::default();
        let x = Scalar::random(&mut rng);
        // mvk <- g2^x
        let mvk = MVK(G2Affine::one() * x);
        // k1 <- H_G1("PoP"||mvk)^x
        let k1 = hash_to_g1(&[POP, &mvk.to_bytes()].concat()) * x;
        // k2 <- g1^x
        let k2 = G1Affine::one() * x;
        // return sk,mvk,k=(k1,k2)
        (SK(x), PK { mvk, k1, k2 })

    }

    pub fn check(pk: &PK) -> bool {
        // if e(k1,g2) = e(H_G1("PoP"||mvk),mvk)
        //      and e(g1,mvk) = e(k2,g2)
        //      are both true, return 1
        let mvk_g2 = G2Affine::from(pk.mvk.0);
        let e_k1_g2   = pairing(pk.k1.into(), G2Affine::one());
        let h_pop_mvk = hash_to_g1(&[POP, &pk.mvk.to_bytes()].concat());
        let e_hg1_mvk = pairing(h_pop_mvk, mvk_g2);

        let e_g1_mvk = pairing(G1Affine::one(), mvk_g2);
        let e_k2_g2  = pairing(pk.k2.into(), G2Affine::one());

        (e_k1_g2 == e_hg1_mvk) && (e_g1_mvk == e_k2_g2)
    }

    pub fn sig(sk: &SK, msg: &[u8]) -> Sig {
        // return sigma <- H_G1("M"||msg)^x
        let g1 = hash_to_g1(&[M, msg].concat());
        Sig(G1Affine::one() * sk.0)
    }

    pub fn ver(msg: &[u8], mvk: &MVK, sigma: &Sig) -> bool {
        // return 1 if e(sigma,g2) = e(H_G1("M"||msg),mvk)
        let e_sigma_g2 = pairing(G1Affine::from(sigma.0), G2Affine::one());
        let e_hg1_mvk  = pairing(hash_to_g1(&[M, msg].concat()), G2Affine::from(mvk.0));

        e_sigma_g2 == e_hg1_mvk
    }

    // MSP.AKey
    pub fn aggregate_keys(mvks: &[MVK]) -> MVK {
        MVK(mvks
            .iter()
            .fold(G2Projective::from(G2Affine::zero()),
                  |acc, x| acc + x.0))
    }

    // MSP.Aggr
    pub fn aggregate_sigs(msg: &[u8], sigmas: &[Sig]) -> Sig {
        // XXX: what is d?
        Sig(sigmas
            .iter()
            .fold(G1Projective::from(G1Affine::zero()),
                  |acc, s| acc + s.0))
    }

    // MSP.AVer
    pub fn aggregate_ver(msg: &[u8], ivk: &MVK, mu: &Sig) -> bool {
        Self::ver(msg, ivk, mu)
    }

    pub fn eval(msg: &[u8], index: Index, sigma: &Sig) -> Unknown {
        // XXX: See section 6 to implement M from Elligator Squared
        // return ev <- M_msg,index(sigma)
        // H("map"||msg||index||sigma)
        unimplemented!()
    }
}

impl MVK {
    pub fn to_bytes(&self) -> [u8; 96] {
        // Notes: to_vec() here causes a segfault later, why?
        self.0.to_uncompressed()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pairing() {
        // Sanity check that the library is correct and we are using it correctly.
        let mut rng = OsRng::default();
        for _ in 0..128 {
            let x = Scalar::random(&mut rng);
            let y = Scalar::random(&mut rng);
            let gt = pairing((G1Affine::one() * x).into(), (G2Affine::one() * y).into());
            let should_be = pairing((G1Affine::one() * (x * y)).into(), G2Affine::one());
        }
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = MSP::gen();
            assert!(MSP::check(&pk));
        }
    }

    #[test]
    fn test_sig() {
        for _ in 0..1 {
            let (sk, pk) = MSP::gen();
            let msg = rand::random::<[u8;16]>();
            let sig = MSP::sig(&sk, &msg);
            assert!(MSP::ver(&msg, &pk.mvk, &sig));
        }
    }

    #[test]
    fn test_aggregate_sig() {
        for _ in 0..128 {
            let msg = rand::random::<[u8;16]>();
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..16 {
                let (sk, pk) = MSP::gen();
                let sig = MSP::sig(&sk, &msg);
                assert!(MSP::ver(&msg, &pk.mvk, &sig));
                sigs.push(sig);
                mvks.push(pk.mvk);
            }
            let ivk = MSP::aggregate_keys(&mvks);
            let mu = MSP::aggregate_sigs(&msg, &sigs);
            assert!(MSP::aggregate_ver(&msg, &ivk, &mu));
        }
    }
}
