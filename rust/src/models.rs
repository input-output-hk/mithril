//! Convenient instantiations MTHashLeaf

pub mod digest {
    //! Implementations necessary for Digest-based instantiations

    use crate::merkle_tree::MTHashLeaf;
    use crate::stm::MTValue;
    use ark_ec::PairingEngine;
    use ark_ff::ToBytes;
    use ark_std::io::Write;
    use sha3::Digest;

    impl<T: ark_ff::ToBytes, D: Digest> MTHashLeaf<T> for D {
        type F = Vec<u8>;

        fn new() -> Self {
            Self::new()
        }

        fn inject(&mut self, v: &T) -> Self::F {
            ark_ff::to_bytes!(v).unwrap()
        }

        fn zero() -> Self::F {
            vec![0]
        }

        fn as_bytes(h: &Self::F) -> Vec<u8> {
            h.to_vec()
        }

        fn hash_children(&mut self, left: &Self::F, right: &Self::F) -> Self::F {
            let input: &[u8] = &[&left[..], &right[..]].concat();

            D::digest(input)[..].to_vec()
        }
    }

    impl<PE: PairingEngine> ToBytes for MTValue<PE> {
        fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
            self.0 .0.write(&mut writer)?;
            self.1.write(&mut writer)
        }
    }
}
