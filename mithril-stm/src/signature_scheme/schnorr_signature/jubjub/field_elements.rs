use anyhow::anyhow;
use dusk_jubjub::{Fq as JubjubBase, Fr as JubjubScalar};
use ff::Field;
use rand_core::{CryptoRng, RngCore};

use super::ProjectivePoint;
use crate::{StmResult, signature_scheme::SchnorrSignatureError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BaseFieldElement(pub(crate) JubjubBase);

impl BaseFieldElement {
    pub(crate) fn add(&self, other: &Self) -> Self {
        BaseFieldElement(self.0 + other.0)
    }

    pub(crate) fn sub(&self, other: &Self) -> Self {
        BaseFieldElement(self.0 - other.0)
    }

    pub(crate) fn mul(&self, other: &Self) -> Self {
        BaseFieldElement(self.0 * other.0)
    }

    pub(crate) fn square(&self) -> Self {
        BaseFieldElement(self.0.square())
    }

    pub(crate) fn get_one() -> Self {
        BaseFieldElement(JubjubBase::ONE)
    }

    pub(crate) fn collect_coordinates_of_list_of_points(
        point_list: &[ProjectivePoint],
    ) -> Vec<Self> {
        let mut coordinates: Vec<BaseFieldElement> = Vec::new();
        for point in point_list {
            let (u, v) = point.get_coordinates();
            coordinates.push(u);
            coordinates.push(v);
        }
        coordinates
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ScalarFieldElement(pub(crate) JubjubScalar);

impl ScalarFieldElement {
    pub(crate) fn new_random_scalar(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        ScalarFieldElement(JubjubScalar::random(rng))
    }

    pub(crate) fn is_zero(&self) -> bool {
        if self.0 == JubjubScalar::zero() {
            return true;
        }
        false
    }

    pub(crate) fn is_one(&self) -> bool {
        if self.0 == JubjubScalar::one() {
            return true;
        }
        false
    }

    pub(crate) fn new_random_nonzero_scalar(
        rng: &mut (impl RngCore + CryptoRng),
    ) -> StmResult<Self> {
        for _ in 0..100 {
            let random_scalar = Self::new_random_scalar(rng);
            if !random_scalar.is_zero() {
                return Ok(random_scalar);
            }
        }
        Err(anyhow!(SchnorrSignatureError::RandomScalarGenerationError))
    }

    pub(crate) fn sub(&self, other: &Self) -> Self {
        ScalarFieldElement(self.0 - other.0)
    }

    pub(crate) fn mul(&self, other: &Self) -> Self {
        ScalarFieldElement(self.0 * other.0)
    }

    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut scalar_bytes = [0u8; 32];
        scalar_bytes.copy_from_slice(
            bytes
                .get(..32)
                .ok_or(SchnorrSignatureError::ScalarFieldElementSerializationError)?,
        );

        match JubjubScalar::from_bytes(&scalar_bytes).into_option() {
            Some(scalar_field_element) => Ok(Self(scalar_field_element)),
            None => Err(anyhow!(
                SchnorrSignatureError::ScalarFieldElementSerializationError
            )),
        }
    }
}
