//! Off-circuit accumulator and verification-key fixed-base helpers for the recursive IVC circuit.

use std::collections::BTreeMap;

use ff::Field;
use group::Group;

use super::{
    Accumulator, ConstraintSystem, EmulatedCurve, KZGCommitmentScheme, Msm, NativeField,
    PairingEngine, RecursiveEmulation, VerifyingKey, verifier,
};

pub(crate) fn trivial_accumulator(fixed_base_names: &[String]) -> Accumulator<RecursiveEmulation> {
    Accumulator::<RecursiveEmulation>::new(
        Msm::new(
            &[EmulatedCurve::default()],
            &[NativeField::ONE],
            &BTreeMap::new(),
        ),
        Msm::new(
            &[EmulatedCurve::default()],
            &[NativeField::ONE],
            &fixed_base_names
                .iter()
                .map(|name| (name.clone(), NativeField::ZERO))
                .collect(),
        ),
    )
}

pub(crate) fn fixed_bases_and_names(
    vk_name: &str,
    vk: &VerifyingKey<NativeField, KZGCommitmentScheme<PairingEngine>>,
) -> (BTreeMap<String, EmulatedCurve>, Vec<String>) {
    let mut fixed_bases = BTreeMap::new();
    fixed_bases.insert(String::from("com_instance"), EmulatedCurve::identity());
    fixed_bases.extend(verifier::fixed_bases::<RecursiveEmulation>(vk_name, vk));
    let fixed_base_names = fixed_bases.keys().cloned().collect::<Vec<_>>();
    (fixed_bases, fixed_base_names)
}

pub(crate) fn fixed_base_names(vk_name: &str, cs: &ConstraintSystem<NativeField>) -> Vec<String> {
    let mut fixed_base_names = vec![String::from("com_instance")];
    fixed_base_names.extend(verifier::fixed_base_names::<RecursiveEmulation>(
        vk_name,
        cs.num_fixed_columns() + cs.num_selectors(),
        cs.permutation().columns.len(),
    ));
    fixed_base_names
}
