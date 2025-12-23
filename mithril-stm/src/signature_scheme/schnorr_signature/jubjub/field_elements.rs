use anyhow::anyhow;
use dusk_jubjub::{Fq as JubjubBase, Fr as JubjubScalar};
use ff::Field;
use rand_core::{CryptoRng, RngCore};
use std::ops::{Add, Mul, Sub};

use crate::{StmResult, signature_scheme::SchnorrSignatureError};

/// Represents an element in the base field of the Jubjub curve
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BaseFieldElement(pub(crate) JubjubBase);

impl BaseFieldElement {
    /// Retrieves the multiplicative identity element of the base field
    pub(crate) fn get_one() -> Self {
        BaseFieldElement(JubjubBase::ONE)
    }
}

impl Add for BaseFieldElement {
    type Output = BaseFieldElement;

    /// Adds two base field elements
    fn add(self, other: BaseFieldElement) -> BaseFieldElement {
        BaseFieldElement(self.0 + other.0)
    }
}

impl Sub for &BaseFieldElement {
    type Output = BaseFieldElement;

    /// Subtracts one base field element from another
    fn sub(self, other: &BaseFieldElement) -> BaseFieldElement {
        BaseFieldElement(self.0 - other.0)
    }
}

impl Mul for BaseFieldElement {
    type Output = BaseFieldElement;

    /// Multiplies two base field elements
    fn mul(self, other: BaseFieldElement) -> BaseFieldElement {
        BaseFieldElement(self.0 * other.0)
    }
}

impl Mul for &BaseFieldElement {
    type Output = BaseFieldElement;

    /// Multiplies a base field element by another base field element
    fn mul(self, other: &BaseFieldElement) -> BaseFieldElement {
        BaseFieldElement(self.0 * other.0)
    }
}

/// Represents an element in the scalar field of the Jubjub curve
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ScalarFieldElement(pub(crate) JubjubScalar);

impl ScalarFieldElement {
    /// Generates a new random scalar field element
    pub(crate) fn new_random_scalar(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        ScalarFieldElement(JubjubScalar::random(rng))
    }

    /// Checks if the scalar field element is zero
    pub(crate) fn is_zero(&self) -> bool {
        if self.0 == JubjubScalar::zero() {
            return true;
        }
        false
    }

    /// Checks if the scalar field element is one
    pub(crate) fn is_one(&self) -> bool {
        if self.0 == JubjubScalar::one() {
            return true;
        }
        false
    }

    /// Tries to generate a new random non-zero scalar field element in 100 attempts
    ///
    /// Returns an error if unable to generate a non-zero scalar after 100 attempts
    pub(crate) fn new_random_nonzero_scalar(
        rng: &mut (impl RngCore + CryptoRng),
    ) -> StmResult<Self> {
        for _ in 0..100 {
            let random_scalar = Self::new_random_scalar(rng);
            if !random_scalar.is_zero() {
                return Ok(random_scalar);
            }
        }
        Err(anyhow!(SchnorrSignatureError::RandomScalarGeneration))
    }

    /// Converts the scalar field element to its byte representation
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Constructs a scalar field element from its byte representation
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut scalar_bytes = [0u8; 32];
        scalar_bytes.copy_from_slice(
            bytes
                .get(..32)
                .ok_or(SchnorrSignatureError::ScalarFieldElementSerialization)?,
        );

        match JubjubScalar::from_bytes(&scalar_bytes).into_option() {
            Some(scalar_field_element) => Ok(Self(scalar_field_element)),
            None => Err(anyhow!(
                SchnorrSignatureError::ScalarFieldElementSerialization
            )),
        }
    }
}

impl Mul for ScalarFieldElement {
    type Output = ScalarFieldElement;

    /// Multiplies two scalar field elements
    fn mul(self, other: ScalarFieldElement) -> ScalarFieldElement {
        ScalarFieldElement(self.0 * other.0)
    }
}

impl Sub for ScalarFieldElement {
    type Output = ScalarFieldElement;

    /// Subtracts one scalar field element from another
    fn sub(self, other: ScalarFieldElement) -> ScalarFieldElement {
        ScalarFieldElement(self.0 - other.0)
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::*;

    mod golden {
        use super::*;

        const GOLDEN_JSON: &str = r#"[126, 191, 239, 197, 88, 151, 248, 254, 187, 143, 86, 35, 29, 62, 90, 13, 196, 71, 234, 5, 90, 124, 205, 194, 51, 192, 228, 133, 25, 140, 157, 7]"#;

        fn golden_value() -> ScalarFieldElement {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap()
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

    mod base_field_arithmetic {
        use super::*;

        #[test]
        fn test_add() {
            let a = BaseFieldElement(JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE);
            let result = a + b;
            assert_eq!(result, BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE));
        }

        #[test]
        fn test_add_with_zero() {
            let a = BaseFieldElement(JubjubBase::ONE);
            let zero = BaseFieldElement(JubjubBase::zero());
            let result = a.clone() + zero;
            assert_eq!(result, a);
        }

        #[test]
        fn test_sub_references() {
            let a = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE);
            let result = &a - &b;
            assert_eq!(result, BaseFieldElement(JubjubBase::ONE));
        }

        #[test]
        fn test_sub_same_values() {
            let a = BaseFieldElement(JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE);
            let result = &a - &b;
            assert_eq!(result, BaseFieldElement(JubjubBase::zero()));
        }

        #[test]
        fn test_mul_owned() {
            let a = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE);
            let result = a * b;
            let expected = JubjubBase::ONE + JubjubBase::ONE;
            assert_eq!(result, BaseFieldElement(expected * expected));
        }

        #[test]
        fn test_mul_with_one() {
            let a = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE);
            let one = BaseFieldElement::get_one();
            let result = a.clone() * one;
            assert_eq!(result, a);
        }

        #[test]
        fn test_mul_with_zero() {
            let a = BaseFieldElement(JubjubBase::ONE);
            let zero = BaseFieldElement(JubjubBase::zero());
            let result = a * zero;
            assert_eq!(result, BaseFieldElement(JubjubBase::zero()));
        }

        #[test]
        fn test_mul_references() {
            let a = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE + JubjubBase::ONE);
            let result = &a * &b;
            let expected = (JubjubBase::ONE + JubjubBase::ONE)
                * (JubjubBase::ONE + JubjubBase::ONE + JubjubBase::ONE);
            assert_eq!(result, BaseFieldElement(expected));
        }

        #[test]
        fn test_chained_operations() {
            let a = BaseFieldElement(JubjubBase::ONE);
            let b = BaseFieldElement(JubjubBase::ONE);
            let c = BaseFieldElement(JubjubBase::ONE);
            let result = (a + b) * c;
            assert_eq!(result, BaseFieldElement(JubjubBase::ONE + JubjubBase::ONE));
        }
    }

    mod scalar_field_arithmetic {
        use super::*;

        #[test]
        fn test_mul() {
            let mut rng = ChaCha20Rng::from_seed([1u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let b = ScalarFieldElement(JubjubScalar::one());
            let result = a * b;
            assert_eq!(result, a);
        }

        #[test]
        fn test_mul_with_zero() {
            let mut rng = ChaCha20Rng::from_seed([2u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let zero = ScalarFieldElement(JubjubScalar::zero());
            let result = a * zero;
            assert!(result.is_zero());
        }

        #[test]
        fn test_mul_associativity() {
            let mut rng = ChaCha20Rng::from_seed([3u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let b = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let c = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let result1 = (a * b) * c;
            let result2 = a * (b * c);
            assert_eq!(result1, result2);
        }

        #[test]
        fn test_sub() {
            let mut rng = ChaCha20Rng::from_seed([4u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let result = a - a;
            assert!(result.is_zero());
        }

        #[test]
        fn test_sub_with_zero() {
            let mut rng = ChaCha20Rng::from_seed([5u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let zero = ScalarFieldElement(JubjubScalar::zero());
            let result = a - zero;
            assert_eq!(result, a);
        }

        #[test]
        fn test_sub_specific_values() {
            let two = ScalarFieldElement(JubjubScalar::one() + JubjubScalar::one());
            let one = ScalarFieldElement(JubjubScalar::one());
            let result = two - one;
            assert!(result.is_one());
        }

        #[test]
        fn test_combined_operations() {
            let mut rng = ChaCha20Rng::from_seed([6u8; 32]);
            let a = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let b = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();
            let c = ScalarFieldElement::new_random_nonzero_scalar(&mut rng).unwrap();

            let left = a * (b - c);
            let right = (a * b) - (a * c);
            assert_eq!(left, right);
        }
    }
}
