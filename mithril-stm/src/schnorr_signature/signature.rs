pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};

pub struct SchnorrSignature {
    sigma: JubjubSubgroup,
    s: JubjubScalar,
    c: JubjubBase,
}
