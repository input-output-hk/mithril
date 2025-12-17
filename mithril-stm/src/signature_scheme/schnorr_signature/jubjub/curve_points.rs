use anyhow::anyhow;
use dusk_jubjub::{
    AffinePoint as JubjubAffinePoint, EDWARDS_D, ExtendedPoint as JubjubExtended,
    SubgroupPoint as JubjubSubgroup,
};
use group::{Group, GroupEncoding};
use std::ops::{Add, Mul};

use super::{BaseFieldElement, ScalarFieldElement};
use crate::{StmResult, signature_scheme::SchnorrSignatureError};

#[derive(Clone)]
pub(crate) struct AffinePoint(JubjubAffinePoint);

impl AffinePoint {
    pub(crate) fn from_projective_point(projective_point: ProjectivePoint) -> Self {
        AffinePoint(JubjubAffinePoint::from(projective_point.0))
    }

    pub(crate) fn get_u(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_u())
    }

    pub(crate) fn get_v(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_v())
    }
}

impl From<&PrimeOrderProjectivePoint> for AffinePoint {
    fn from(prime_order_projective_point: &PrimeOrderProjectivePoint) -> Self {
        AffinePoint(JubjubAffinePoint::from(JubjubExtended::from(
            prime_order_projective_point.0,
        )))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ProjectivePoint(pub(crate) JubjubExtended);

impl ProjectivePoint {
    pub(crate) fn hash_to_projective_point(input: &[u8]) -> Self {
        ProjectivePoint(JubjubExtended::hash_to_point(input))
    }

    pub(crate) fn get_coordinates(&self) -> (BaseFieldElement, BaseFieldElement) {
        let affine_point = AffinePoint::from_projective_point(*self);

        (affine_point.get_u(), affine_point.get_v())
    }

    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut projective_point_bytes = [0u8; 32];
        projective_point_bytes
            .copy_from_slice(bytes.get(..32).ok_or(SchnorrSignatureError::SerializationError)?);

        match JubjubExtended::from_bytes(&projective_point_bytes).into_option() {
            Some(projective_point) => Ok(Self(projective_point)),
            None => Err(anyhow!(SchnorrSignatureError::ProjectivePointSerialization)),
        }
    }

    pub(crate) fn is_prime_order(self) -> bool {
        self.0.is_prime_order().into()
    }
}

impl Add for ProjectivePoint {
    type Output = ProjectivePoint;

    fn add(self, other: ProjectivePoint) -> ProjectivePoint {
        ProjectivePoint(self.0 + other.0)
    }
}

impl Mul<ProjectivePoint> for ScalarFieldElement {
    type Output = ProjectivePoint;

    fn mul(self, point: ProjectivePoint) -> ProjectivePoint {
        ProjectivePoint(point.0 * self.0)
    }
}

impl From<PrimeOrderProjectivePoint> for ProjectivePoint {
    fn from(prime_order_projective_point: PrimeOrderProjectivePoint) -> Self {
        ProjectivePoint(JubjubExtended::from(prime_order_projective_point.0))
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct PrimeOrderProjectivePoint(pub(crate) JubjubSubgroup);

impl PrimeOrderProjectivePoint {
    pub(crate) fn create_generator() -> Self {
        PrimeOrderProjectivePoint(JubjubSubgroup::generator())
    }

    /// Check if the given point is on the curve using its coordinates
    pub(crate) fn is_on_curve(&self) -> StmResult<PrimeOrderProjectivePoint> {
        let point_affine_representation = AffinePoint::from(self);
        let (x, y) = (
            point_affine_representation.get_u(),
            point_affine_representation.get_v(),
        );
        let x_square = &x * &x;
        let y_square = &y * &y;

        let lhs = &y_square - &x_square;
        let rhs = (x_square * y_square) * BaseFieldElement(EDWARDS_D) + BaseFieldElement::get_one();

        if lhs != rhs {
            return Err(anyhow!(SchnorrSignatureError::PointIsNotOnCurve(Box::new(
                *self
            ))));
        }
        Ok(*self)
    }

    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut prime_order_projective_point_bytes = [0u8; 32];
        prime_order_projective_point_bytes
            .copy_from_slice(bytes.get(..32).ok_or(SchnorrSignatureError::SerializationError)?);

        match JubjubSubgroup::from_bytes(&prime_order_projective_point_bytes).into_option() {
            Some(prime_order_projective_point) => Ok(Self(prime_order_projective_point)),
            None => Err(anyhow!(
                SchnorrSignatureError::PrimeOrderProjectivePointSerialization
            )),
        }
    }
}

impl Add for PrimeOrderProjectivePoint {
    type Output = PrimeOrderProjectivePoint;

    fn add(self, other: PrimeOrderProjectivePoint) -> PrimeOrderProjectivePoint {
        PrimeOrderProjectivePoint(self.0 + other.0)
    }
}

impl Mul<PrimeOrderProjectivePoint> for ScalarFieldElement {
    type Output = PrimeOrderProjectivePoint;

    fn mul(self, point: PrimeOrderProjectivePoint) -> PrimeOrderProjectivePoint {
        PrimeOrderProjectivePoint(point.0 * self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod golden {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::*;

        const GOLDEN_JSON: &str = r#"[144, 52, 95, 161, 127, 253, 49, 32, 140, 217, 231, 207, 32, 238, 244, 196, 97, 241, 47, 95, 101, 9, 70, 136, 194, 66, 187, 253, 200, 32, 218, 43]"#;

        fn golden_value() -> PrimeOrderProjectivePoint {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let point = PrimeOrderProjectivePoint::create_generator();
            PrimeOrderProjectivePoint(point.0 * scalar.0)
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
