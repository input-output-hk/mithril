//! Reexports and type aliases for the Halo2 prototype (kept close to mithril-circuits to minimize diff).

pub use midnight_curves::{
    Bls12, Fq as JubjubBase, Fr as JubjubScalar, JubjubAffine, JubjubExtended as Jubjub,
    JubjubSubgroup,
};

pub type Target = JubjubBase;
pub type Msg = JubjubBase;
pub type MerkleRoot = JubjubBase;
pub type LotteryIndex = u32;
