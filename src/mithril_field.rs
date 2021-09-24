//! Abstractions for working with ark curves
use crate::hashutils::hash_message;

use ark_ec::{
    models::short_weierstrass_jacobian::{GroupAffine, GroupProjective},
    AffineCurve, SWModelParameters,
};

use ark_ff::{Field, FpParameters, PrimeField};

pub trait HashToCurve: AffineCurve {
    fn hash_to_curve(input: &[u8]) -> Self;
}

impl<C: AffineCurve> HashToCurve for C {
    fn hash_to_curve(bytes: &[u8]) -> Self {
        let needed =
            <<<C::BaseField as Field>::BasePrimeField as PrimeField>::Params as FpParameters>::MODULUS_BITS / 8;
        let x: &[u8] = &hash_message(bytes, needed as usize);
        let mut q = num_bigint::BigUint::from_bytes_le(x);
        loop {
            if let Some(elt) = Self::from_random_bytes(&q.to_bytes_le()) {
                return elt;
            } else {
                q += 1u8;
            }
        }
    }
}

pub trait AsCoord<T> {
    fn as_coords(&self) -> Vec<T>;
}

impl<P: SWModelParameters> AsCoord<P::BaseField> for GroupProjective<P> {
    fn as_coords(&self) -> Vec<P::BaseField> {
        vec![self.x, self.y, self.z]
    }
}

impl<P: SWModelParameters> AsCoord<P::BaseField> for GroupAffine<P> {
    fn as_coords(&self) -> Vec<P::BaseField> {
        vec![self.x, self.y]
    }
}

/// For interpreting Ark Fields as FFF fields (necessary to use neptune's poseidon implementation)
pub mod wrapper {
    use ark_ff::fields::{
        FftParameters, FpParameters, LegendreSymbol, PrimeField, SquareRootField,
    };
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt;

    #[derive(PartialEq, Eq, Clone, Copy, Debug)]
    pub struct MithrilFieldWrapper<F>(pub F);

    impl<F> Serialize for MithrilFieldWrapper<F> {
        fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            todo!()
        }
    }

    impl<F> Deserialize<'static> for MithrilFieldWrapper<F> {
        fn deserialize<D>(_: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'static>,
        {
            todo!()
        }
    }

    impl<F> Serialize for MithrilFieldWrapperRepr<F> {
        fn serialize<S>(&self, _: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            todo!()
        }
    }

    impl<F> Deserialize<'static> for MithrilFieldWrapperRepr<F> {
        fn deserialize<D>(_: D) -> std::result::Result<Self, D::Error>
        where
            D: Deserializer<'static>,
        {
            todo!()
        }
    }

    pub trait MithrilField: PrimeField + SquareRootField {}

    impl<F> MithrilField for F where F: PrimeField + SquareRootField {}

    impl<F> fmt::Display for MithrilFieldWrapper<F> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            unimplemented!()
        }
    }

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Debug)]
    pub struct MithrilFieldWrapperRepr<R>(pub R);

    impl<R: ark_ff::BigInteger> fmt::Display for MithrilFieldWrapperRepr<R> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            unimplemented!()
        }
    }

    impl<R: ark_ff::BigInteger> AsRef<[u64]> for MithrilFieldWrapperRepr<R> {
        fn as_ref(&self) -> &[u64] {
            self.0.as_ref()
        }
    }

    impl<R: ark_ff::BigInteger> AsMut<[u64]> for MithrilFieldWrapperRepr<R> {
        fn as_mut(&mut self) -> &mut [u64] {
            self.0.as_mut()
        }
    }

    impl<R: ark_ff::BigInteger> From<u64> for MithrilFieldWrapperRepr<R> {
        fn from(x: u64) -> Self {
            MithrilFieldWrapperRepr(R::from(x))
        }
    }

    impl<R> fff::PrimeFieldRepr for MithrilFieldWrapperRepr<R>
    where
        R: ark_ff::BigInteger,
    {
        fn sub_noborrow(&mut self, other: &Self) {
            self.0.sub_noborrow(&other.0);
        }
        fn add_nocarry(&mut self, other: &Self) {
            self.0.add_nocarry(&other.0);
        }
        fn num_bits(&self) -> u32 {
            self.0.num_bits()
        }
        fn is_zero(&self) -> bool {
            self.0.is_zero()
        }
        fn is_odd(&self) -> bool {
            self.0.is_odd()
        }
        fn is_even(&self) -> bool {
            self.0.is_even()
        }
        fn div2(&mut self) {
            self.0.div2()
        }
        fn shr(&mut self, amt: u32) {
            self.0.divn(amt)
        }
        fn mul2(&mut self) {
            self.0.mul2()
        }
        fn shl(&mut self, amt: u32) {
            self.0.muln(amt)
        }
    }

    impl<F> fff::Field for MithrilFieldWrapper<F>
    where
        F: MithrilField,
    {
        fn random<R>(rng: &mut R) -> Self
        where
            R: rand_core::RngCore,
        {
            loop {
                let v = &[
                    rng.next_u64().to_le_bytes(),
                    rng.next_u64().to_le_bytes(),
                    rng.next_u64().to_le_bytes(),
                    rng.next_u64().to_le_bytes(),
                ]
                .concat();
                if let Some(x) = F::from_random_bytes(v) {
                    return MithrilFieldWrapper(x);
                }
            }
        }

        fn zero() -> Self {
            MithrilFieldWrapper(F::zero())
        }

        fn one() -> Self {
            MithrilFieldWrapper(F::one())
        }

        fn is_zero(&self) -> bool {
            self.0.is_zero()
        }

        fn square(&mut self) {
            self.0.square_in_place();
        }

        fn double(&mut self) {
            self.0.double_in_place();
        }

        fn negate(&mut self) {
            self.0 = self.0.neg()
        }

        fn add_assign(&mut self, rhs: &Self) {
            self.0.add_assign(rhs.0)
        }

        fn sub_assign(&mut self, rhs: &Self) {
            self.0.sub_assign(rhs.0)
        }

        fn mul_assign(&mut self, rhs: &Self) {
            self.0.mul_assign(rhs.0)
        }

        fn inverse(&self) -> std::option::Option<Self> {
            self.0.inverse().map(MithrilFieldWrapper)
        }

        fn frobenius_map(&mut self, power: usize) {
            self.0.frobenius_map(power)
        }
    }

    impl<F> fff::PrimeField for MithrilFieldWrapper<F>
    where
        F: MithrilField,
    {
        type Repr = MithrilFieldWrapperRepr<F::BigInt>;
        fn from_repr(
            repr: MithrilFieldWrapperRepr<F::BigInt>,
        ) -> std::result::Result<Self, fff::PrimeFieldDecodingError> {
            if let Some(me) = F::from_repr(repr.0) {
                Ok(MithrilFieldWrapper(me))
            } else {
                Err(fff::PrimeFieldDecodingError::NotInField(
                    "MithrilFieldWrapper repr not valid".to_string(),
                ))
            }
        }

        fn into_repr(&self) -> <Self as fff::PrimeField>::Repr {
            let x = self.0.into_repr();
            MithrilFieldWrapperRepr(x)
        }

        fn char() -> <Self as fff::PrimeField>::Repr {
            MithrilFieldWrapperRepr(F::Params::MODULUS)
        }

        fn multiplicative_generator() -> Self {
            MithrilFieldWrapper(F::multiplicative_generator())
        }

        fn root_of_unity() -> Self {
            MithrilFieldWrapper(F::get_root_of_unity((2 as usize).pow(Self::S)).unwrap())
            // TODO: Boop
        }

        fn from_random_bytes(bytes: &[u8]) -> std::option::Option<Self> {
            F::from_random_bytes(bytes).map(MithrilFieldWrapper)
        }

        const NUM_BITS: u32 = F::Params::MODULUS_BITS; //TODO ???
        const S: u32 = F::Params::TWO_ADICITY;
        const CAPACITY: u32 = F::Params::CAPACITY;
    }

    impl<F> fff::SqrtField for MithrilFieldWrapper<F>
    where
        F: MithrilField,
        // MithrilFieldWrapper<F>: fff::Field,
    {
        fn legendre(&self) -> fff::LegendreSymbol {
            match self.0.legendre() {
                LegendreSymbol::Zero => fff::LegendreSymbol::Zero,
                LegendreSymbol::QuadraticNonResidue => fff::LegendreSymbol::QuadraticNonResidue,
                LegendreSymbol::QuadraticResidue => fff::LegendreSymbol::QuadraticResidue,
            }
        }

        fn sqrt(&self) -> std::option::Option<Self> {
            self.0.sqrt().map(MithrilFieldWrapper)
        }
    }

    impl<F> From<MithrilFieldWrapper<F>> for MithrilFieldWrapperRepr<F::BigInt>
    where
        F: MithrilField,
    {
        fn from(fld: MithrilFieldWrapper<F>) -> Self {
            MithrilFieldWrapperRepr(fld.0.into_repr())
        }
    }

    #[derive(Clone)]
    pub struct MithrilEngine<F: MithrilField> {
        ph: std::marker::PhantomData<F>,
    }

    impl<F> fff::ScalarEngine for MithrilEngine<F>
    where
        F: 'static + Clone + MithrilField,
    {
        type Fr = MithrilFieldWrapper<F>;
    }
}
