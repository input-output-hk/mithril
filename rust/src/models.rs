//! Convenient instantiations MTHashLeaf

pub mod digest {
    //! Implementations necessary for Digest-based instantiations

    use std::convert::TryInto;

    use crate::merkle_tree::MTHashLeaf;
    use crate::stm::MTValue;
    use ark_ec::PairingEngine;
    use ark_ff::{FromBytes, ToBytes};
    use ark_std::io::{Read, Write};
    use blake2::Digest;

    /// A newtype that allows us to implement traits
    /// like ToBytes, FromBytes
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct DigestHash(pub(crate) Vec<u8>);

    impl<T: ark_ff::ToBytes, D: Digest> MTHashLeaf<T> for D {
        type F = DigestHash;

        fn new() -> Self {
            Self::new()
        }

        fn inject(&mut self, v: &T) -> Self::F {
            DigestHash(ark_ff::to_bytes!(v).unwrap())
        }

        fn zero() -> Self::F {
            DigestHash(vec![0])
        }

        fn as_bytes(h: &Self::F) -> Vec<u8> {
            h.0.to_vec()
        }

        fn hash_children(&mut self, left: &Self::F, right: &Self::F) -> Self::F {
            let input: &[u8] = &[&left.0[..], &right.0[..]].concat();

            DigestHash(D::digest(input)[..].to_vec())
        }
    }

    impl ToBytes for DigestHash {
        fn write<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
            let n: u64 = self.0.len().try_into().unwrap();
            n.write(&mut writer)?;
            for b in &self.0 {
                b.write(&mut writer)?;
            }

            Ok(())
        }
    }

    impl FromBytes for DigestHash {
        fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
            let n = u64::read(&mut reader)?;
            let mut bytes = Vec::with_capacity(n as usize);
            for _ in 0..n {
                let b = u8::read(&mut reader)?;
                bytes.push(b);
            }

            Ok(DigestHash(bytes))
        }
    }

    impl<PE: PairingEngine> ToBytes for MTValue<PE> {
        fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
            self.0 .0.write(&mut writer)?;
            self.1.write(&mut writer)
        }
    }
}
