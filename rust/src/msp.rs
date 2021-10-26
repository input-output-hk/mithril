//! Base multisignature scheme.

use super::mithril_curves::hash_to_curve;
use super::Index;

use ark_ec::{AffineCurve, PairingEngine};
use ark_ff::{bytes::ToBytes, ToConstraintField, UniformRand};
use blake2::VarBlake2b;
use digest::{Update, VariableOutput};
use rand::Rng;
use std::cmp::Ordering;
use std::hash::Hash;
use std::marker::PhantomData;

pub struct Msp<PE: PairingEngine> {
    x: PhantomData<PE>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MspSk<PE: PairingEngine>(PE::Fr);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MspMvk<PE: PairingEngine>(pub PE::G2Projective);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MspPk<PE: PairingEngine> {
    pub mvk: MspMvk<PE>,
    pub k1: PE::G1Projective,
    pub k2: PE::G1Projective,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MspSig<PE: PairingEngine>(pub(crate) PE::G1Projective);

impl<PE: PairingEngine> MspSig<PE>
where
    PE::G1Projective: ToConstraintField<PE::Fq>,
{
    fn cmp_msp_sig(&self, other: &Self) -> Ordering {
        self.0.to_field_elements().cmp(&other.0.to_field_elements())
    }
}

impl<PE: PairingEngine> PartialOrd for MspSig<PE>
where
    PE::G1Projective: ToConstraintField<PE::Fq>,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_sig(other))
    }
}

impl<PE: PairingEngine> Ord for MspSig<PE>
where
    PE::G1Projective: ToConstraintField<PE::Fq>,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_sig(other)
    }
}

const POP: &[u8] = b"PoP";
const M: &[u8] = b"M";

impl<PE: PairingEngine> Msp<PE> {
    pub fn gen<R>(rng: &mut R) -> (MspSk<PE>, MspPk<PE>)
    where
        R: Rng + rand::CryptoRng + ?Sized,
    {
        // sk=x <- Zq
        // mvk <- g2^x
        let x = <PE::Fr as UniformRand>::rand(rng);
        let mvk = MspMvk(PE::G2Affine::prime_subgroup_generator().mul(x));
        // k1 <- H_G1("PoP"||mvk)^x
        let k1 = hash_to_curve::<PE::G1Affine>([POP, &mvk.to_bytes()].concat().as_ref()).mul(x);
        // k2 <- g1^x
        let k2 = PE::G1Affine::prime_subgroup_generator().mul(x);
        // return sk,mvk,k=(k1,k2)
        (MspSk(x), MspPk { mvk, k1, k2 })
    }

    pub fn check(pk: &MspPk<PE>) -> bool {
        // if e(k1,g2) = e(H_G1("PoP"||mvk),mvk)
        //      and e(g1,mvk) = e(k2,g2)
        //      are both true, return 1
        let mvk_g2 = PE::G2Affine::from(pk.mvk.0);
        let e_k1_g2 = PE::pairing(pk.k1.into(), PE::G2Affine::prime_subgroup_generator());
        let h_pop_mvk = hash_to_curve::<PE::G1Affine>([POP, &pk.mvk.to_bytes()].concat().as_ref());
        let e_hg1_mvk = PE::pairing(h_pop_mvk, mvk_g2);

        let e_g1_mvk = PE::pairing(PE::G1Affine::prime_subgroup_generator(), mvk_g2);
        let e_k2_g2 = PE::pairing(pk.k2.into(), PE::G2Affine::prime_subgroup_generator());

        (e_k1_g2 == e_hg1_mvk) && (e_g1_mvk == e_k2_g2)
    }

    pub fn sig(sk: &MspSk<PE>, msg: &[u8]) -> MspSig<PE> {
        // return sigma <- H_G1("M"||msg)^x
        let g1 = hash_to_curve::<PE::G1Affine>([M, msg].concat().as_ref());
        MspSig(g1.mul(sk.0))
    }

    pub fn ver(msg: &[u8], mvk: &MspMvk<PE>, sigma: &MspSig<PE>) -> bool {
        // return 1 if e(sigma,g2) = e(H_G1("M"||msg),mvk)
        let e_sigma_g2 = PE::pairing(
            PE::G1Affine::from(sigma.0),
            PE::G2Affine::prime_subgroup_generator(),
        );
        let g1 = hash_to_curve::<PE::G1Affine>([M, msg].concat().as_ref());
        let e_hg1_mvk = PE::pairing(g1, PE::G2Affine::from(mvk.0));

        e_sigma_g2 == e_hg1_mvk
    }

    // MSP.AKey
    pub fn aggregate_keys(mvks: &[MspMvk<PE>]) -> MspMvk<PE> {
        MspMvk(mvks.iter().map(|s| s.0).sum())
    }

    // MSP.Aggr
    pub fn aggregate_sigs(sigmas: &[MspSig<PE>]) -> MspSig<PE> {
        MspSig(sigmas.iter().map(|s| s.0).sum())
    }

    // MSP.AVer
    pub fn aggregate_ver(msg: &[u8], ivk: &MspMvk<PE>, mu: &MspSig<PE>) -> bool {
        Self::ver(msg, ivk, mu)
    }

    pub fn eval(msg: &[u8], index: Index, sigma: &MspSig<PE>) -> u64 {
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
        let mut dest = [0; 8];
        hasher.finalize_variable(|out| {
            dest.copy_from_slice(out);
        });
        u64::from_le_bytes(dest)
        // // XXX: See section 6 to implement M from Elligator Squared
        // // return ev <- M_msg,index(sigma)
    }
}

impl<PE: PairingEngine> MspMvk<PE> {
    pub fn to_bytes(&self) -> Vec<u8> {
        ark_ff::to_bytes!(self.0).unwrap()
    }
}

impl<PE: PairingEngine> MspSig<PE> {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![];
        self.0.write(&mut bytes).unwrap();
        bytes
    }
}

mod to_bytes {
    use super::*;
    use ark_ff::bytes::{FromBytes, ToBytes};
    use ark_std::io::{Read, Write};

    impl<PE: PairingEngine> FromBytes for MspSk<PE> {
        fn read<R: Read>(reader: R) -> std::result::Result<Self, std::io::Error> {
            let k = <PE::Fr as FromBytes>::read(reader)?;
            Ok(MspSk(k))
        }
    }

    impl<PE: PairingEngine> FromBytes for MspMvk<PE> {
        fn read<R: Read>(reader: R) -> std::result::Result<Self, std::io::Error> {
            let k = <PE::G2Projective as FromBytes>::read(reader)?;
            Ok(MspMvk(k))
        }
    }

    impl<PE: PairingEngine> FromBytes for MspPk<PE> {
        fn read<R: Read>(mut reader: R) -> std::result::Result<Self, std::io::Error> {
            let mvk = MspMvk::<PE>::read(&mut reader)?;
            let k1 = PE::G1Projective::read(&mut reader)?;
            let k2 = PE::G1Projective::read(&mut reader)?;
            Ok(MspPk { mvk, k1, k2 })
        }
    }
    impl<PE: PairingEngine> FromBytes for MspSig<PE> {
        fn read<R: Read>(reader: R) -> std::result::Result<Self, std::io::Error> {
            let s = <PE::G1Projective as FromBytes>::read(reader)?;
            Ok(MspSig(s))
        }
    }

    impl<PE: PairingEngine> ToBytes for MspSk<PE> {
        fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
            self.0.write(&mut writer)
        }
    }

    impl<PE: PairingEngine> ToBytes for MspMvk<PE> {
        fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
            self.0.write(&mut writer)
        }
    }

    impl<PE: PairingEngine> ToBytes for MspPk<PE> {
        fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
            self.mvk.write(&mut writer)?;
            self.k1.write(&mut writer)?;
            self.k2.write(&mut writer)
        }
    }
    impl<PE: PairingEngine> ToBytes for MspSig<PE> {
        fn write<W: Write>(&self, writer: W) -> std::result::Result<(), std::io::Error> {
            self.0.write(writer)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ark_bls12_377::{Bls12_377, Fr, G1Affine, G2Affine};
    use ark_ff::FromBytes;
    use proptest::prelude::*;
    use rand::thread_rng;
    use rand_core::SeedableRng;

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
            let (sk, pk) = Msp::<Bls12_377>::gen(&mut thread_rng());
            let sig = Msp::sig(&sk, &msg);
            assert!(Msp::ver(&msg, &pk.mvk, &sig));
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                            r in any::<u64>()) {
            let (sk, pk) = Msp::<Bls12_377>::gen(&mut thread_rng());
            let x = MspSig(G1Affine::prime_subgroup_generator().mul(Fr::from(r)));
            assert!(!Msp::ver(&msg, &pk.mvk, &x));
        }

        #[test]
        fn test_aggregate_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                              num_sigs in 1..16) {
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..num_sigs {
                let (sk, pk) = Msp::<Bls12_377>::gen(&mut thread_rng());
                let sig = Msp::sig(&sk, &msg);
                assert!(Msp::ver(&msg, &pk.mvk, &sig));
                sigs.push(sig);
                mvks.push(pk.mvk);
            }
            let ivk = Msp::aggregate_keys(&mvks);
            let mu = Msp::aggregate_sigs(&sigs);
            assert!(Msp::aggregate_ver(&msg, &ivk, &mu));
        }

        #[test]
        fn test_eval_sanity_check(msg in prop::collection::vec(any::<u8>(), 1..128),
                                  idx in any::<u64>(),
                                  s in any::<u64>()) {
            let sigma = MspSig(G1Affine::prime_subgroup_generator().mul(Fr::from(s)));
            Msp::<Bls12_377>::eval(&msg, idx, &sigma);
        }


        #[test]
        fn serialize_deserialize_pk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let (_, pk) = Msp::<Bls12_377>::gen(&mut rng);
            let pk_bytes: &[u8] = &ark_ff::to_bytes!(pk).unwrap();
            let pk2: MspPk<Bls12_377> = MspPk::read(pk_bytes).unwrap();
            assert!(pk == pk2);
        }

        #[test]
        fn serialize_deserialize_sk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let (sk, _) = Msp::<Bls12_377>::gen(&mut rng);
            let sk_bytes: &[u8] = &ark_ff::to_bytes!(sk).unwrap();
            let sk2: MspSk<Bls12_377> = MspSk::read(sk_bytes).unwrap();
            assert!(sk == sk2);
        }
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = Msp::<Bls12_377>::gen(&mut thread_rng());
            assert!(Msp::check(&pk));
        }
    }
}
