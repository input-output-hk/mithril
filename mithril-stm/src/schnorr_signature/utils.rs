use anyhow::{Context, Ok, anyhow};
use dusk_jubjub::{
    AffinePoint as JubjubAffine, EDWARDS_D, ExtendedPoint as JubjubExtended, Fq as JubjubBase,
    Fr as JubjubScalar,
};
use ff::Field;

use crate::{StmResult, error::SchnorrSignatureError};

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

/// Extract the coordinates of a given point in an Extended form
///
/// This is mainly used to feed the Poseidon hash function
pub fn get_coordinates_extended(point: JubjubExtended) -> (JubjubBase, JubjubBase) {
    let point_affine_representation = JubjubAffine::from(point);
    let x_coordinate = point_affine_representation.get_u();
    let y_coordinate = point_affine_representation.get_v();

    (x_coordinate, y_coordinate)
}

/// Extract the coordinates of given points in an Extended form
///
/// This is mainly used to feed the Poseidon hash function, the order is maintained
/// from input to output which is important for the hash function
pub fn get_coordinates_several_points(points: &[JubjubExtended]) -> Vec<JubjubBase> {
    let mut points_coordinates = vec![];
    for p in points {
        let point_affine_representation = JubjubAffine::from(p);
        let x_coordinate = point_affine_representation.get_u();
        let y_coordinate = point_affine_representation.get_v();
        points_coordinates.push(x_coordinate);
        points_coordinates.push(y_coordinate);
    }
    points_coordinates
}

/// Convert an element of the BLS12-381 base field to one of the Jubjub base field
pub fn jubjub_base_to_scalar(x: &JubjubBase) -> StmResult<JubjubScalar> {
    let bytes = x.to_bytes();

    if bytes.len() < 32 {
        return Err(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Not enough bytes to convert to a jubjub scalar");
    }

    Ok(JubjubScalar::from_raw([
        u64::from_le_bytes(
            bytes[0..8]
                .try_into()
                .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Failed to convert bls scalar to jubjub scalar")?,
        ),
        u64::from_le_bytes(
            bytes[8..16]
                .try_into()
                .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Failed to convert bls scalar to jubjub scalar")?,
        ),
        u64::from_le_bytes(
            bytes[16..24]
                .try_into()
                .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Failed to convert bls scalar to jubjub scalar")?,
        ),
        u64::from_le_bytes(
            bytes[24..32]
                .try_into()
                .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Failed to convert bls scalar to jubjub scalar")?,
        ),
    ]))
}

#[cfg(test)]
mod test {

    use super::*;
    use dusk_jubjub::{JubJubAffine, JubJubExtended, SubgroupPoint as JubjubSubgroup};
    use group::Group;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn get_coordinates_extended_from_point() {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let point = JubjubSubgroup::random(&mut rng);

        let (x, y) = get_coordinates_extended(point.into());
        let point_subgroup = JubjubSubgroup::from_raw_unchecked(x, y);

        assert_eq!(point, point_subgroup);
    }

    #[test]
    fn get_coordinates_from_several_points() {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let points = vec![
            JubJubExtended::random(&mut rng),
            JubJubExtended::random(&mut rng),
            JubJubExtended::random(&mut rng),
        ];

        let coordinates = get_coordinates_several_points(&points);

        let mut coordinates_iter = coordinates.iter();
        for i in 0..3 {
            let x = coordinates_iter.next().unwrap();
            let y = coordinates_iter.next().unwrap();
            let point_affine = JubJubAffine::from_raw_unchecked(*x, *y);
            let point_extended = JubJubExtended::from_affine(point_affine);
            assert_eq!(points[i], point_extended);
        }
    }
}
