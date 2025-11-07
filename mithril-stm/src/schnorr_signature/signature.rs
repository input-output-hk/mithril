use anyhow::{Result, anyhow};
use midnight_circuits::hash::poseidon::PoseidonChip;
use midnight_circuits::instructions::HashToCurveCPU;
use midnight_circuits::instructions::hash::HashCPU;
use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};

use group::Group;

use crate::schnorr_signature::{
    DST_SIGNATURE, JubjubHashToCurve, get_coordinates, hash_msg_to_jubjubbase,
    verification_key::SchnorrVerificationKey,
};

/// Structure of the Schnorr signature to use with the SNARK
/// This signature includes a value `sigma` which depends only on
/// the message and the signing key.
/// This value is used in the lottery process to determine the correct indices.
pub(crate) struct SchnorrSignature {
    pub(crate) sigma: JubjubSubgroup,
    pub(crate) s: JubjubScalar,
    pub(crate) c: JubjubBase,
}

impl SchnorrSignature {
    pub(crate) fn verify(&self, msg: &[u8], vk: &SchnorrVerificationKey) -> Result<()> {
        let g = JubjubSubgroup::generator();

        // First hashing the message to a scalar then hashing it to a curve point
        let hash = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)?]);

        // Computing R1 = H(msg) * s + sigma * c
        let c_bytes = self.c.to_bytes_le();
        let c_scalar = JubjubScalar::from_raw([
            u64::from_le_bytes(c_bytes[0..8].try_into()?),
            u64::from_le_bytes(c_bytes[8..16].try_into()?),
            u64::from_le_bytes(c_bytes[16..24].try_into()?),
            u64::from_le_bytes(c_bytes[24..32].try_into()?),
        ]);
        let h_s = hash * self.s;
        let sigma_c = self.sigma * c_scalar;
        let r1_tilde = h_s + sigma_c;

        // Computing R2 = g * s + vk * c
        let g_s = g * self.s;
        let vk_c = vk.0 * c_scalar;
        let r2_tilde = g_s + vk_c;

        let (hashx, hashy) = get_coordinates(hash);
        let (vkx, vky) = get_coordinates(vk.0);
        let (sigmax, sigmay) = get_coordinates(self.sigma);
        let (r1x, r1y) = get_coordinates(r1_tilde);
        let (r2x, r2y) = get_coordinates(r2_tilde);

        let c_tilde = PoseidonChip::<JubjubBase>::hash(&[
            DST_SIGNATURE,
            hashx,
            hashy,
            vkx,
            vky,
            sigmax,
            sigmay,
            r1x,
            r1y,
            r2x,
            r2y,
        ]);

        if c_tilde != self.c {
            // TODO: Wrong error for now, need to change that once the errors are added
            return Err(anyhow!("Signature failed to verify."));
        }

        Ok(())
    }
}
