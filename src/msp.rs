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
pub struct Sig(Unknown);

impl MVK {
    fn hash_to_g1(&self, string: &str) -> G1Affine {
        // k1 <- H_G1("PoP"||mvk)^x
        unimplemented!()
    }
}

impl MSP {
    pub fn gen() -> (SK, PK) {
        // sk=x <- Zq
        let mut rng = OsRng::default();
        let x = Scalar::random(&mut rng);
        // mvk <- g2^x
        let mvk = MVK(G2Affine::one() * x);
        // k1 <- H_G1("PoP"||mvk)^x
        let h = mvk.hash_to_g1("PoP");
        let k1 = h * x;
        // k2 <- g1^x
        let k2 = G1Affine::one() * x;
        // return sk,mvk,k=(k1,k2)
        (SK(x), PK { mvk, k1, k2 })
    }

    pub fn check(pk: &PK) -> bool {
        // if e(k1,g2) = e(H_G1("PoP"||mvk),mvk)
        //      and e(g1,mvk) = e(k2,g2)
        //      are both true, return 1
        pairing(pk.k1.into(), G2Affine::one());
        unimplemented!()
    }

    pub fn sig(sk: &SK, msg: &[u8]) -> Sig {
        // return sigma <- H_G1("M"||msg)^x
        unimplemented!()
    }

    pub fn ver(msg: &[u8], mvk: &MVK, sigma: &Sig) -> bool {
        // return 1 if e(sigma,g2) = e(H_G1("M"||msg),mvk)
        unimplemented!()
    }

    // MSP.AKey
    pub fn aggregate_keys(mvks: &[MVK]) -> MVK {
        // ivk = product(mvk_i)
        unimplemented!()
    }

    // MSP.Aggr
    pub fn aggregate_sigs(msg: &[u8], sigmas: &[Sig]) -> Sig {
        // XXX: what is d?
        // mu <- product_1^d(sigmas)
        unimplemented!()
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
    pub fn to_bytes(&self) -> Vec<u8> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = MSP::gen();
            assert!(MSP::check(&pk));
        }
    }

    #[test]
    fn test_sig() {
        for _ in 0..128 {
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
