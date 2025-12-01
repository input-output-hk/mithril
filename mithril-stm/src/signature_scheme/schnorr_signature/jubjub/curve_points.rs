use anyhow::anyhow;
use dusk_jubjub::{
    AffinePoint as JubjubAffinePoint, EDWARDS_D, ExtendedPoint as JubjubExtended,
    SubgroupPoint as JubjubSubgroup,
};
use group::{Group, GroupEncoding};

use super::{BaseFieldElement, ScalarFieldElement};
use crate::{StmResult, signature_scheme::SchnorrSignatureError};

#[derive(Clone)]
pub(crate) struct AffinePoint(JubjubAffinePoint);

impl AffinePoint {
    pub(crate) fn from_projective_point(projective_point: ProjectivePoint) -> Self {
        AffinePoint(JubjubAffinePoint::from(projective_point.0))
    }

    pub(crate) fn from_prime_order_projective_point(
        prime_order_projective_point: &PrimeOrderProjectivePoint,
    ) -> Self {
        AffinePoint(JubjubAffinePoint::from(
            ProjectivePoint::from_prime_order_projective_point(*prime_order_projective_point).0,
        ))
    }

    pub(crate) fn get_u(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_u())
    }

    pub(crate) fn get_v(&self) -> BaseFieldElement {
        BaseFieldElement(self.0.get_v())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ProjectivePoint(pub(crate) JubjubExtended);

impl ProjectivePoint {
    pub(crate) fn hash_to_projective_point(input: &[u8]) -> Self {
        ProjectivePoint(JubjubExtended::hash_to_point(input))
    }

    pub(crate) fn add(&self, other: Self) -> Self {
        ProjectivePoint(self.0 + other.0)
    }

    pub(crate) fn scalar_multiplication(&self, scalar: &ScalarFieldElement) -> Self {
        ProjectivePoint(self.0 * scalar.0)
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
            None => Err(anyhow!(
                SchnorrSignatureError::ProjectivePointSerializationError
            )),
        }
    }

    pub(crate) fn from_prime_order_projective_point(
        prime_order_projective_point: PrimeOrderProjectivePoint,
    ) -> Self {
        ProjectivePoint(JubjubExtended::from(prime_order_projective_point.0))
    }

    pub(crate) fn is_prime_order(self) -> bool {
        self.0.is_prime_order().into()
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct PrimeOrderProjectivePoint(pub(crate) JubjubSubgroup);

impl PrimeOrderProjectivePoint {
    pub(crate) fn create_generator() -> Self {
        PrimeOrderProjectivePoint(JubjubSubgroup::generator())
    }

    pub(crate) fn add(&self, other: Self) -> Self {
        PrimeOrderProjectivePoint(self.0 + other.0)
    }

    pub(crate) fn scalar_multiplication(&self, scalar: &ScalarFieldElement) -> Self {
        PrimeOrderProjectivePoint(self.0 * scalar.0)
    }

    /// Check if the given point is on the curve using its coordinates
    pub(crate) fn is_on_curve(&self) -> StmResult<PrimeOrderProjectivePoint> {
        let point_affine_representation = AffinePoint::from_prime_order_projective_point(self);
        let (x, y) = (
            point_affine_representation.get_u(),
            point_affine_representation.get_v(),
        );
        let x_square = x.square();
        let y_square = y.square();

        let lhs = y_square.sub(&x_square);
        let mut rhs = x_square.mul(&y_square);
        rhs = rhs.mul(&BaseFieldElement(EDWARDS_D));
        rhs = rhs.add(&BaseFieldElement::get_one());

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
                SchnorrSignatureError::PrimeOrderProjectivePointSerializationError
            )),
        }
    }
}
