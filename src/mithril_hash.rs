use crate::mithril_field::{
    wrapper::{MithrilEngine, MithrilField, MithrilFieldWrapper, MithrilFieldWrapperRepr},
    AsCoord,
};
use neptune::Poseidon;

pub type MithrilHasher<'a, F> = Poseidon<'a, MithrilEngine<F>, typenum::U2>;

pub trait IntoHash<F: MithrilField> {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F>;
}

////////////////////////
// IntoHash Instances //
////////////////////////

impl<F: MithrilField> IntoHash<F> for u64 {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        hasher.reset();
        let repr = MithrilFieldWrapperRepr(F::BigInt::from(*self));
        let mf = fff::PrimeField::from_repr(repr).unwrap();
        hasher.input(mf).unwrap();
        hasher.hash()
    }
}

impl<F: MithrilField> IntoHash<F> for MithrilFieldWrapper<F> {
    fn into_hash<'a>(&self, _mh: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        *self
    }
}

impl<F: MithrilField, V: IntoHash<F> + Clone> IntoHash<F> for &[V] {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        self.to_vec().into_hash(hasher)
    }
}

impl<F: MithrilField, V: IntoHash<F>> IntoHash<F> for Vec<V> {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        assert!(self.len() > 0, "Can not convert empty slice to Hash");
        let mut h: MithrilFieldWrapper<F> = self[0].into_hash(hasher);
        for val in self {
            let h2: MithrilFieldWrapper<F> = val.into_hash(hasher);
            hasher.set_preimage(&[h, h2]);
            h = hasher.hash()
        }

        h
    }
}

impl<F: MithrilField, V1: IntoHash<F>, V2: IntoHash<F>> IntoHash<F> for (V1, V2) {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        let h1 = self.0.into_hash(hasher);
        let h2 = self.1.into_hash(hasher);
        hasher.reset();
        hasher.input(h1).unwrap();
        hasher.input(h2).unwrap();
        hasher.hash()
    }
}

impl<F: MithrilField, V: IntoHash<F>> IntoHash<F> for Option<V> {
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        if let Some(inner) = self {
            inner.into_hash(hasher)
        } else {
            0u64.into_hash(hasher)
        }
    }
}

// Ark Curve instances
use ark_ec::models::{short_weierstrass_jacobian::GroupProjective, SWModelParameters};

impl<P: SWModelParameters, F: MithrilField> IntoHash<F> for GroupProjective<P>
where
    P::BaseField: IntoHash<F>,
    GroupProjective<P>: AsCoord<P::BaseField>,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        self.as_coords().into_hash(hasher)
    }
}

// Ark Field Instances
use ark_ff::fields::models::{
    cubic_extension::{CubicExtField, CubicExtParameters},
    quadratic_extension::{QuadExtField, QuadExtParameters},
    Fp256, Fp320, Fp384, Fp448, Fp64, Fp768, Fp832,
};

impl<P: QuadExtParameters, F: MithrilField> IntoHash<F> for QuadExtField<P>
where
    P::BaseField: IntoHash<F>,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        vec![self.c0.into_hash(hasher), self.c1.into_hash(hasher)].into_hash(hasher)
    }
}

impl<P: CubicExtParameters, F: MithrilField> IntoHash<F> for CubicExtField<P>
where
    P::BaseField: IntoHash<F>,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        vec![
            self.c0.into_hash(hasher),
            self.c1.into_hash(hasher),
            self.c2.into_hash(hasher),
        ]
        .into_hash(hasher)
    }
}

macro_rules! fp_into_hash {
    ($x:ident) => {
        impl<P, F: MithrilField> IntoHash<F> for $x<P> {
            fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
                self.0.as_ref().into_hash(hasher)
            }
        }
    };
}

fp_into_hash!(Fp64);
fp_into_hash!(Fp256);
fp_into_hash!(Fp320);
fp_into_hash!(Fp384);
fp_into_hash!(Fp448);
fp_into_hash!(Fp768);
fp_into_hash!(Fp832);

impl<F: MithrilField> IntoHash<F> for num_bigint::BigUint
where
    Vec<u64>: IntoHash<F>,
{
    fn into_hash<'a>(&self, hasher: &mut MithrilHasher<'a, F>) -> MithrilFieldWrapper<F> {
        self.to_u64_digits().into_hash(hasher)
    }
}
