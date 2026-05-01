use anyhow::{Context, anyhow};

use crate::StmResult;

use super::{
    BaseFieldElement, DOMAIN_SEPARATION_TAG_STANDARD_SIGNATURE, PrimeOrderProjectivePoint,
    ProjectivePoint, ScalarFieldElement, SchnorrSignatureError, SchnorrVerificationKey,
    compute_poseidon_digest,
};

/// Structure of the standard (non-unique) Schnorr signature to use with the SNARK.
///
/// This signature consists of only a `response` and a `challenge`. It is used
/// by the SNARK genesis certificate, which does not require the
/// per-signer uniqueness tag carried by `UniqueSchnorrSignature`.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct StandardSchnorrSignature {
    /// Part of the standard Schnorr signature depending on the signing key
    pub(crate) response: ScalarFieldElement,
    /// Part of the standard Schnorr signature NOT depending on the signing key
    pub(crate) challenge: BaseFieldElement,
}

impl StandardSchnorrSignature {
    /// Verify a standard Schnorr signature against a verification key.
    pub fn verify(
        &self,
        msg: &[BaseFieldElement],
        verification_key: &SchnorrVerificationKey,
    ) -> StmResult<()> {
        verification_key
            .is_valid()
            .with_context(|| "Signature verification failed due to invalid verification key")?;

        let prime_order_generator_point = PrimeOrderProjectivePoint::create_generator();

        let challenge_as_scalar = ScalarFieldElement::from_base_field(&self.challenge)?;

        let random_point_recomputed =
            self.response * prime_order_generator_point + challenge_as_scalar * verification_key.0;

        let mut points_coordinates: Vec<BaseFieldElement> =
            vec![DOMAIN_SEPARATION_TAG_STANDARD_SIGNATURE];
        points_coordinates.extend(
            [
                ProjectivePoint::from(verification_key.0),
                ProjectivePoint::from(random_point_recomputed),
            ]
            .iter()
            .flat_map(|point| {
                let (u, v) = point.get_coordinates();
                [u, v]
            }),
        );

        points_coordinates.extend_from_slice(msg);
        let challenge_recomputed = compute_poseidon_digest(&points_coordinates);

        if challenge_recomputed != self.challenge {
            return Err(anyhow!(SchnorrSignatureError::StandardSignatureInvalid(
                Box::new(*self)
            )));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{BaseFieldElement, SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn valid_signature_verification() {
        let msg = vec![0, 0, 0, 1];
        let base_input = BaseFieldElement::try_from(msg.as_slice()).unwrap();
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone());

        let sig = sk.sign_standard(&[base_input], &mut rng).unwrap();

        sig.verify(&[base_input], &vk)
            .expect("Valid signature should verify successfully");
    }
}
