use dusk_jubjub::{
    AffinePoint as JubjubAffine, EDWARDS_D, ExtendedPoint as JubjubExtended, Fq as JubjubBase,
    Fr as JubjubScalar,
};

use ff::Field;

use anyhow::{Context, Ok, anyhow};

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
/// This is mainly use to feed the Poseidon hash function
pub fn get_coordinates_extended(point: JubjubExtended) -> (JubjubBase, JubjubBase) {
    let point_affine_representation = JubjubAffine::from(point);
    let x_coordinate = point_affine_representation.get_u();
    let y_coordinate = point_affine_representation.get_v();

    (x_coordinate, y_coordinate)
}

/// Extract the coordinates of given points in an Extended form
///
/// This is mainly use to feed the Poseidon hash function, the order is maintained
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
