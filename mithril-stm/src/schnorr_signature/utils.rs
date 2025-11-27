use anyhow::{Context, anyhow};
use dusk_jubjub::{
    AffinePoint as JubjubAffine, EDWARDS_D, ExtendedPoint as JubjubExtended, Fq as JubjubBase,
    Fr as JubjubScalar,
};
use ff::Field;
use group::Curve;
use rand_core::{CryptoRng, RngCore};

use crate::{StmResult, schnorr_signature::SchnorrSignatureError};

/// Check if the given point is on the curve using its coordinates
pub fn is_on_curve(point: JubjubExtended) -> bool {
    let point_affine_representation = JubjubAffine::from(point);
    let (x, y) = (
        point_affine_representation.get_u(),
        point_affine_representation.get_v(),
    );
    let x_square = x.square();
    let y_square = y.square();

    let lhs = y_square - x_square;
    let rhs = JubjubBase::ONE + EDWARDS_D * x_square * y_square;

    lhs == rhs
}

/// Extract the coordinates of given points in an Extended form
///
/// This is mainly used to feed the Poseidon hash function, the order is maintained
/// from input to output which is important for the hash function
pub fn get_coordinates_several_points(points: &[JubjubExtended]) -> Vec<(JubjubBase, JubjubBase)> {
    let mut points_affine =
        vec![JubjubAffine::from_raw_unchecked(JubjubBase::ZERO, JubjubBase::ZERO); points.len()];
    JubjubExtended::batch_normalize(points, &mut points_affine);

    points_affine
        .into_iter()
        .map(|p| (p.get_u(), p.get_v()))
        .collect::<Vec<(JubjubBase, JubjubBase)>>()
}

/// Generate a random non zero value from the scalar field of Jubjub
///
/// Tries to generate 100 times a non zero value and returns an error if it fails to do so
pub fn generate_non_zero_scalar<R: RngCore + CryptoRng>(rng: &mut R) -> StmResult<JubjubScalar> {
    for _ in 0..100 {
        let random_scalar = JubjubScalar::random(&mut *rng);
        if random_scalar != JubjubScalar::ZERO {
            return Ok(random_scalar);
        }
    }
    Err(anyhow!(SchnorrSignatureError::RandomScalarGenerationError))
        .with_context(|| "Failed to generate a non zero signing key after 100 attempts.")
}

#[cfg(test)]
mod tests {

    use super::*;
    use dusk_jubjub::{AffinePoint as JubjubAffine, ExtendedPoint as JubjubExtended};
    use group::Group;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn get_coordinates_from_several_points() {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let points = vec![
            JubjubExtended::random(&mut rng),
            JubjubExtended::random(&mut rng),
            JubjubExtended::random(&mut rng),
        ];

        let coordinates = get_coordinates_several_points(&points);

        let mut coordinates_iter = coordinates.iter();
        for p in points.iter().take(3) {
            let (x, y) = coordinates_iter.next().unwrap();
            let point_affine = JubjubAffine::from_raw_unchecked(*x, *y);
            let point_extended = JubjubExtended::from_affine(point_affine);
            assert_eq!(*p, point_extended);
        }
    }

    #[test]
    fn generation_non_zero_scalar() {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);

        let scalar = generate_non_zero_scalar(&mut rng).unwrap();

        assert_ne!(scalar, JubjubScalar::ZERO);
    }
}
