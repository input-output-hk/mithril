use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};

/// Structure of the Schnorr signature to use with the SNARK
/// This signature includes a value `sigma` which depends only on
/// the message and the signing key.
/// This value is used in the lottery process to determine the correct indices.
pub(crate) struct SchnorrSignature {
    pub(crate) sigma: JubjubSubgroup,
    pub(crate) s: JubjubScalar,
    pub(crate) c: JubjubBase,
}
