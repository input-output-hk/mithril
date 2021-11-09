#![warn(missing_docs)]

//! Implementation of Mithril Threshold Stake-Based Signatures
//! [[paper](https://eprint.iacr.org/2021/916)].

pub mod key_reg;
pub mod merkle_tree;
mod mithril_curves;
pub mod mithril_proof;
pub mod models;
pub mod msp;
pub mod proof;
pub mod stm;
pub mod c_api;

use ark_ff::{FromBytes, ToBytes};
use std::{
    convert::TryInto,
    io::{Read, Write},
};

use crate::merkle_tree::{MTHashLeaf, MerkleTree};

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Party identifier, unique for each participant in the protocol.
pub type PartyId = usize;

/// Quorum index for signatures.
/// An aggregate signature (`StmMultiSig`) must have at least `k` unique indices.
pub type Index = u64;

/// Path of hashes from root to leaf in a Merkle Tree.
/// Used to verify the credentials of users and signatures.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path<F>(Vec<F>);

/// Compares the output of `phi` (a real) to the output of `ev` (a hash).
/// Used to determine winning lottery tickets.
pub fn ev_lt_phi(phi_f: f64, ev: u64, stake: Stake, total_stake: Stake) -> bool {
    //TODO: Fix this, casting to f64 isn't safe
    let w = (stake as f64) / (total_stake as f64);
    let phi = 1.0 - (1.0 - phi_f).powf(w);
    let ev_as_f64 = ev as f64 / 2_f64.powf(64.0);
    // println!("{} {}", phi, ev_as_f64);
    ev_as_f64 < phi
}

/// Serializes the Merkle Tree together with a message in a single vector of bytes.
/// Outputs msg || avk as a vector of bytes.
pub fn concat_avk_with_msg<L, H>(avk: &MerkleTree<L, H>, msg: &[u8]) -> Vec<u8>
where
    H: MTHashLeaf<L>,
{
    let mut msgp = msg.to_vec();
    let mut bytes = avk.root_to_bytes();
    msgp.append(&mut bytes);

    msgp
}

impl<F: ToBytes> ToBytes for Path<F> {
    fn write<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
        let n: u64 = self.0.len().try_into().unwrap();
        n.write(&mut writer)?;
        for pi in &self.0 {
            pi.write(&mut writer)?;
        }

        Ok(())
    }
}
impl<F: FromBytes> FromBytes for Path<F> {
    fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let n = u64::read(&mut reader)?;
        let mut p = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let pi = F::read(&mut reader)?;
            p.push(pi);
        }

        Ok(Path(p))
    }
}
