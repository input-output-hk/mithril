#![allow(dead_code, unused_variables)]

pub mod hashutils;
pub mod key_reg;
pub mod merkle_tree;
pub mod mithril_curves;
pub mod mithril_proof;
pub mod models;
pub mod msp;
pub mod proof;
pub mod stm;

/// The quantity of stake held by a party, represented as a `u64`.
pub type Stake = u64;

/// Party identifier, unique for each participant in the protocol.
pub type PartyId = usize;

/// Quorum index for signatures.
/// An aggregate signature (`StmMutliSig`) must have at least `k` unique indices.
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
