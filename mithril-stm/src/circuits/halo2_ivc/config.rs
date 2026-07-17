use crate::circuits::halo2_ivc::RECURSIVE_CIRCUIT_DEGREE;

use super::{
    CircuitCurve, ComposableChip, ConstraintSystem, EccChip, EccConfig, EmulatedCurve,
    EmulatedCurveBaseField, FieldChip, ForeignWeierstrassEccChip, ForeignWeierstrassEccConfig,
    IvcNativeGadget, NB_ARITH_COLS, NB_ARITH_FIXED_COLS, NB_EDWARDS_COLS, NB_POSEIDON_ADVICE_COLS,
    NB_POSEIDON_FIXED_COLS, NativeChip, NativeConfig, NativeField, P2RDecompositionChip,
    P2RDecompositionConfig, PoseidonChip, PoseidonConfig, Pow2RangeChip,
    nb_foreign_ecc_chip_columns,
};
use midnight_circuits::hash::sha256::{
    NB_SHA256_ADVICE_COLS, NB_SHA256_FIXED_COLS, Sha256Chip, Sha256Config,
};

#[derive(Debug, Clone)]
pub struct IvcConfig {
    pub(crate) native_config: NativeConfig,
    pub(crate) core_decomp_config: P2RDecompositionConfig,
    pub(crate) jubjub_config: EccConfig,
    pub(crate) bls12_381_config: ForeignWeierstrassEccConfig<EmulatedCurve>,
    pub(crate) poseidon_config: PoseidonConfig<NativeField>,
    pub(crate) sha256_config: Sha256Config,
}

/// Returns `(nb_advice_cols, nb_fixed_cols)` — the column pool sizes allocated by
/// `configure_ivc_circuit`. Single source of truth shared with `validate_column_counts`.
pub(crate) fn ivc_column_pool_sizes() -> (usize, usize) {
    // unwrap_or(0) never panics: max() returns None only on an empty iterator,
    // but both arrays are non-empty so the fallback is unreachable.
    let nb_advice_cols = [
        NB_ARITH_COLS,
        NB_EDWARDS_COLS,
        NB_POSEIDON_ADVICE_COLS,
        NB_SHA256_ADVICE_COLS,
        nb_foreign_ecc_chip_columns::<NativeField, EmulatedCurve, EmulatedCurve, IvcNativeGadget>(),
    ]
    .into_iter()
    .max()
    .unwrap_or(0);

    let nb_fixed_cols = [NB_ARITH_FIXED_COLS, NB_POSEIDON_FIXED_COLS, NB_SHA256_FIXED_COLS]
        .into_iter()
        .max()
        .unwrap_or(0);

    (nb_advice_cols, nb_fixed_cols)
}

pub fn configure_ivc_circuit(meta: &mut ConstraintSystem<NativeField>) -> IvcConfig {
    let (nb_advice_cols, nb_fixed_cols) = ivc_column_pool_sizes();

    let advice_columns: Vec<_> = (0..nb_advice_cols).map(|_| meta.advice_column()).collect();
    let fixed_columns: Vec<_> = (0..nb_fixed_cols).map(|_| meta.fixed_column()).collect();
    // Committed-instance column: reserved for committed public inputs; this circuit leaves it empty
    // and places its entire public statement in the plaintext instance column below.
    let committed_instance_column = meta.instance_column();
    // Instance column: carries this circuit's public statement (global, state, accumulator).
    let instance_column = meta.instance_column();

    let native_config = NativeChip::configure(
        meta,
        &(
            advice_columns[..NB_ARITH_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
            fixed_columns[..NB_ARITH_FIXED_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
            [committed_instance_column, instance_column],
        ),
    );
    let core_decomp_config = {
        let pow2_config = Pow2RangeChip::configure(meta, &advice_columns[1..NB_ARITH_COLS]);
        P2RDecompositionChip::configure(meta, &(native_config.clone(), pow2_config))
    };

    let jubjub_config = EccChip::<CircuitCurve>::configure(
        meta,
        &advice_columns[..NB_EDWARDS_COLS]
            .try_into()
            .expect("column counts pre-validated by validate_column_counts"),
    );

    let nb_parallel_range_check = NB_ARITH_COLS - 1;
    let max_bit_length = RECURSIVE_CIRCUIT_DEGREE - 1;

    let base_config = FieldChip::<
        NativeField,
        EmulatedCurveBaseField,
        EmulatedCurve,
        IvcNativeGadget,
    >::configure(
        meta,
        &advice_columns,
        nb_parallel_range_check,
        max_bit_length,
    );
    let bls12_381_config = ForeignWeierstrassEccChip::<
        NativeField,
        EmulatedCurve,
        EmulatedCurve,
        IvcNativeGadget,
        IvcNativeGadget,
    >::configure(
        meta,
        &base_config,
        &advice_columns,
        nb_parallel_range_check,
        max_bit_length,
    );

    let poseidon_config = PoseidonChip::configure(
        meta,
        &(
            advice_columns[..NB_POSEIDON_ADVICE_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
            fixed_columns[..NB_POSEIDON_FIXED_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
        ),
    );

    let sha256_config = Sha256Chip::configure(
        meta,
        &(
            advice_columns[..NB_SHA256_ADVICE_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
            fixed_columns[..NB_SHA256_FIXED_COLS]
                .try_into()
                .expect("column counts pre-validated by validate_column_counts"),
        ),
    );

    IvcConfig {
        native_config,
        core_decomp_config,
        jubjub_config,
        bls12_381_config,
        poseidon_config,
        sha256_config,
    }
}
