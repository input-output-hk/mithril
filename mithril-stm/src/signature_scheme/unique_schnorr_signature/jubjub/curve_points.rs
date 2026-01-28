use anyhow::{Ok, anyhow};
use group::{Group, GroupEncoding};
use midnight_circuits::instructions::HashToCurveCPU;
use midnight_circuits::{
    ecc::{hash_to_curve::HashToCurveGadget, native::EccChip},
    hash::poseidon::PoseidonChip,
    types::AssignedNative,
};
use midnight_curves::{
    EDWARDS_D, Fq as JubjubBase, JubjubAffine as JubjubAffinePoint, JubjubExtended, JubjubSubgroup,
};
use sha2::{Digest, Sha256};
use std::ops::{Add, Mul};

use crate::{StmResult, signature_scheme::UniqueSchnorrSignatureError};

use super::{BaseFieldElement, ScalarFieldElement};

/// Defining a type for the CPU hash to curve gadget
pub(crate) type JubjubHashToCurveGadget = HashToCurveGadget<
    JubjubBase,
    JubjubExtended,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<JubjubExtended>,
>;

/// Represents a point in affine coordinates on the Jubjub curve
#[derive(Clone)]
pub(crate) struct AffinePoint(JubjubAffinePoint);

impl AffinePoint {
    /// Converts a projective point to its affine representation
    pub(crate) fn from_projective_point(projective_point: ProjectivePoint) -> Self {
        AffinePoint(JubjubAffinePoint::from(projective_point.0))
    }

    /// Retrieves the u-coordinate of the affine point
    pub(crate) fn get_u(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_u())
    }

    /// Retrieves the v-coordinate of the affine point
    pub(crate) fn get_v(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_v())
    }
}

impl From<&PrimeOrderProjectivePoint> for AffinePoint {
    /// Converts a prime order projective point to its affine representation
    fn from(prime_order_projective_point: &PrimeOrderProjectivePoint) -> Self {
        AffinePoint(JubjubAffinePoint::from(JubjubExtended::from(
            prime_order_projective_point.0,
        )))
    }
}

/// Represents a point in projective coordinates on the Jubjub curve
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ProjectivePoint(pub(crate) JubjubExtended);

impl ProjectivePoint {
    /// Hashes input bytes to a projective point on the Jubjub curve
    /// For now we leave the SHA call in the function since the SHA
    /// function is not used anywhere else. This might change in the future.
    pub(crate) fn hash_to_projective_point(input: &[u8]) -> StmResult<Self> {
        let mut hash = Sha256::new();
        hash.update(input);
        let mut hashed_input = [0u8; 32];
        hashed_input.copy_from_slice(&hash.finalize());
        let scalar_input = JubjubBase::from_raw([
            u64::from_le_bytes(hashed_input[0..8].try_into()?),
            u64::from_le_bytes(hashed_input[8..16].try_into()?),
            u64::from_le_bytes(hashed_input[16..24].try_into()?),
            u64::from_le_bytes(hashed_input[24..32].try_into()?),
        ]);
        let point = JubjubHashToCurveGadget::hash_to_curve(&[scalar_input]);
        Ok(ProjectivePoint(JubjubExtended::from(point)))
    }

    /// Retrieves the (u, v) coordinates of the projective point in affine representation
    pub(crate) fn get_coordinates(&self) -> (BaseFieldElement, BaseFieldElement) {
        let affine_point = AffinePoint::from_projective_point(*self);

        (affine_point.get_u(), affine_point.get_v())
    }

    /// Converts the projective point to its byte representation
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Constructs a projective point from its byte representation
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut projective_point_bytes = [0u8; 32];
        projective_point_bytes
            .copy_from_slice(bytes.get(..32).ok_or(UniqueSchnorrSignatureError::Serialization)?);

        match JubjubExtended::from_bytes(&projective_point_bytes).into_option() {
            Some(projective_point) => Ok(Self(projective_point)),
            None => Err(anyhow!(
                UniqueSchnorrSignatureError::ProjectivePointSerialization
            )),
        }
    }

    /// Checks if the projective point is of prime order
    pub(crate) fn is_prime_order(self) -> bool {
        self.0.is_prime_order().into()
    }
}

impl Add for ProjectivePoint {
    type Output = ProjectivePoint;

    /// Adds two projective points
    fn add(self, other: ProjectivePoint) -> ProjectivePoint {
        ProjectivePoint(self.0 + other.0)
    }
}

impl Mul<ProjectivePoint> for ScalarFieldElement {
    type Output = ProjectivePoint;

    /// Multiplies a projective point by a scalar field element
    /// Returns the resulting projective point
    fn mul(self, point: ProjectivePoint) -> ProjectivePoint {
        ProjectivePoint(point.0 * self.0)
    }
}

impl From<PrimeOrderProjectivePoint> for ProjectivePoint {
    /// Converts a prime order projective point to a projective point
    fn from(prime_order_projective_point: PrimeOrderProjectivePoint) -> Self {
        ProjectivePoint(JubjubExtended::from(prime_order_projective_point.0))
    }
}

/// Represents a point of prime order in projective coordinates on the Jubjub curve
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct PrimeOrderProjectivePoint(pub(crate) JubjubSubgroup);

impl PrimeOrderProjectivePoint {
    /// Creates the generator point of the prime order subgroup
    pub(crate) fn create_generator() -> Self {
        PrimeOrderProjectivePoint(JubjubSubgroup::generator())
    }

    /// Checks if the given point is on the curve using its coordinates
    pub(crate) fn is_on_curve(&self) -> StmResult<PrimeOrderProjectivePoint> {
        let (x, y) = self.get_coordinates();
        let x_square = x * x;
        let y_square = y * y;

        let lhs = &y_square - &x_square;
        let rhs = (x_square * y_square) * BaseFieldElement(EDWARDS_D) + BaseFieldElement::get_one();

        if lhs != rhs {
            return Err(anyhow!(UniqueSchnorrSignatureError::PointIsNotOnCurve(
                Box::new(*self)
            )));
        }
        Ok(*self)
    }

    /// Retrieves the (u, v) coordinates of the prime order projective point in affine representation
    pub(crate) fn get_coordinates(&self) -> (BaseFieldElement, BaseFieldElement) {
        let affine_point = AffinePoint::from(self);

        (affine_point.get_u(), affine_point.get_v())
    }

    /// Tries to create a PrimeOrderProjectivePoint from coordinates and fails
    /// if the corresponding point is not of prime order or on the curve
    pub(crate) fn from_coordinates(u: BaseFieldElement, v: BaseFieldElement) -> StmResult<Self> {
        // TODO: Add check that the point is on the curve
        PrimeOrderProjectivePoint(JubjubSubgroup::from_raw_unchecked(u.0, v.0)).is_on_curve()
    }

    /// Converts the prime order projective point to its byte representation
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Constructs a prime order projective point from its byte representation
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut prime_order_projective_point_bytes = [0u8; 32];
        prime_order_projective_point_bytes
            .copy_from_slice(bytes.get(..32).ok_or(UniqueSchnorrSignatureError::Serialization)?);

        match JubjubSubgroup::from_bytes(&prime_order_projective_point_bytes).into_option() {
            Some(prime_order_projective_point) => Ok(Self(prime_order_projective_point)),
            None => Err(anyhow!(
                UniqueSchnorrSignatureError::PrimeOrderProjectivePointSerialization
            )),
        }
    }
}

impl Add for PrimeOrderProjectivePoint {
    type Output = PrimeOrderProjectivePoint;

    /// Adds two prime order projective points
    fn add(self, other: PrimeOrderProjectivePoint) -> PrimeOrderProjectivePoint {
        PrimeOrderProjectivePoint(self.0 + other.0)
    }
}

impl Mul<PrimeOrderProjectivePoint> for ScalarFieldElement {
    type Output = PrimeOrderProjectivePoint;

    /// Multiplies a prime order projective point by a scalar field element
    /// Returns the resulting prime order projective point
    fn mul(self, point: PrimeOrderProjectivePoint) -> PrimeOrderProjectivePoint {
        PrimeOrderProjectivePoint(point.0 * self.0)
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::*;

    mod golden {
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

    mod golden_hash {
        use super::*;

        const GOLDEN_BYTES: &[u8] = &[
            15, 44, 110, 49, 102, 14, 172, 174, 230, 224, 30, 24, 129, 48, 80, 106, 88, 47, 98,
            132, 180, 50, 8, 88, 48, 33, 149, 193, 129, 151, 209, 239,
        ];

        fn golden_value() -> ProjectivePoint {
            let msg = [255u8; 32];
            ProjectivePoint::hash_to_projective_point(&msg).unwrap()
        }

        #[test]
        fn golden_hash() {
            let value =
                ProjectivePoint::from_bytes(GOLDEN_BYTES).expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);
        }
    }

    mod projective_point_arithmetic {
        use super::*;

        #[test]
        fn test_add() {
            let mut rng = ChaCha20Rng::from_seed([1u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();
            let p1 = scalar1 * point;
            let p2 = scalar2 * point;

            let result = p1 + p2;

            let bytes = result.to_bytes();
            let recovered = ProjectivePoint::from_bytes(&bytes).unwrap();
            assert_eq!(result, recovered);
        }

        #[test]
        fn test_add_identity() {
            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();
            let identity = ProjectivePoint(JubjubExtended::identity());

            let result = point + identity;
            assert_eq!(result, point);
        }

        #[test]
        fn test_add_commutativity() {
            let mut rng = ChaCha20Rng::from_seed([2u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();
            let p1 = scalar1 * point;
            let p2 = scalar2 * point;

            assert_eq!(p1 + p2, p2 + p1);
        }

        #[test]
        fn test_add_associativity() {
            let mut rng = ChaCha20Rng::from_seed([3u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar3 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();
            let p1 = scalar1 * point;
            let p2 = scalar2 * point;
            let p3 = scalar3 * point;

            assert_eq!((p1 + p2) + p3, p1 + (p2 + p3));
        }

        #[test]
        fn test_scalar_mul() {
            let mut rng = ChaCha20Rng::from_seed([4u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();

            let result = scalar * point;

            let bytes = result.to_bytes();
            let recovered = ProjectivePoint::from_bytes(&bytes).unwrap();
            assert_eq!(result, recovered);
        }

        #[test]
        fn test_scalar_mul_distributivity_over_point_addition() {
            let mut rng = ChaCha20Rng::from_seed([5u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let point1 = ProjectivePoint::hash_to_projective_point(b"test_point_1").unwrap();
            let point2 = ProjectivePoint::hash_to_projective_point(b"test_point_2").unwrap();

            let left = scalar * (point1 + point2);
            let right = (scalar * point1) + (scalar * point2);

            assert_eq!(left, right);
        }

        #[test]
        fn test_scalar_mul_associativity() {
            let mut rng = ChaCha20Rng::from_seed([6u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let point = ProjectivePoint::hash_to_projective_point(b"test_point").unwrap();

            let combined_scalar = scalar1 * scalar2;
            let left = combined_scalar * point;
            let right = scalar1 * (scalar2 * point);

            assert_eq!(left, right);
        }
    }

    mod prime_order_projective_point_arithmetic {
        use super::*;

        #[test]
        fn test_add() {
            let mut rng = ChaCha20Rng::from_seed([7u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let generator = PrimeOrderProjectivePoint::create_generator();
            let p1 = scalar1 * generator;
            let p2 = scalar2 * generator;

            let result = p1 + p2;

            let bytes = result.to_bytes();
            let recovered = PrimeOrderProjectivePoint::from_bytes(&bytes).unwrap();
            assert_eq!(result, recovered);
        }

        #[test]
        fn test_add_identity() {
            let mut rng = ChaCha20Rng::from_seed([8u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let generator = PrimeOrderProjectivePoint::create_generator();
            let point = scalar * generator;
            let identity = PrimeOrderProjectivePoint::default();

            let result = point + identity;
            assert_eq!(result, point);
        }

        #[test]
        fn test_add_commutativity() {
            let mut rng = ChaCha20Rng::from_seed([9u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let generator = PrimeOrderProjectivePoint::create_generator();
            let p1 = scalar1 * generator;
            let p2 = scalar2 * generator;

            assert_eq!(p1 + p2, p2 + p1);
        }

        #[test]
        fn test_add_associativity() {
            let mut rng = ChaCha20Rng::from_seed([10u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar3 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let generator = PrimeOrderProjectivePoint::create_generator();
            let p1 = scalar1 * generator;
            let p2 = scalar2 * generator;
            let p3 = scalar3 * generator;

            assert_eq!((p1 + p2) + p3, p1 + (p2 + p3));
        }

        #[test]
        fn test_scalar_mul() {
            let mut rng = ChaCha20Rng::from_seed([11u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let generator = PrimeOrderProjectivePoint::create_generator();

            let result = scalar * generator;

            let bytes = result.to_bytes();
            let recovered = PrimeOrderProjectivePoint::from_bytes(&bytes).unwrap();
            assert_eq!(result, recovered);
        }

        #[test]
        fn test_scalar_mul_by_generator() {
            let scalar = ScalarFieldElement(midnight_curves::Fr::one());
            let generator = PrimeOrderProjectivePoint::create_generator();

            let result = scalar * generator;
            assert_eq!(result, generator);
        }

        #[test]
        fn test_scalar_mul_distributivity_over_point_addition() {
            let mut rng = ChaCha20Rng::from_seed([12u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let generator = PrimeOrderProjectivePoint::create_generator();
            let point1 = scalar1 * generator;
            let point2 = scalar2 * generator;

            let left = scalar * (point1 + point2);
            let right = (scalar * point1) + (scalar * point2);

            assert_eq!(left, right);
        }

        #[test]
        fn test_scalar_mul_associativity() {
            let mut rng = ChaCha20Rng::from_seed([13u8; 32]);
            let scalar1 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let scalar2 = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let generator = PrimeOrderProjectivePoint::create_generator();

            let combined_scalar = scalar1 * scalar2;
            let left = combined_scalar * generator;
            let right = scalar1 * (scalar2 * generator);

            assert_eq!(left, right);
        }

        #[test]
        fn test_point_on_curve() {
            let mut rng = ChaCha20Rng::from_seed([14u8; 32]);
            let scalar = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let generator = PrimeOrderProjectivePoint::create_generator();

            let point = scalar * generator;

            let result = point.is_on_curve().unwrap();
            assert_eq!(result, point);
        }
    }
}
