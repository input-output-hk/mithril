use super::{
    C, CBase, ComposableChip, ConstraintSystem, EccChip, EccConfig, F, FieldChip,
    ForeignEccChip, ForeignEccConfig, Jubjub, NB_ARITH_COLS, NB_ARITH_FIXED_COLS,
    NB_EDWARDS_COLS, NB_POSEIDON_ADVICE_COLS, NB_POSEIDON_FIXED_COLS, NG, NativeChip,
    NativeConfig, P2RDecompositionChip, P2RDecompositionConfig, PoseidonChip, PoseidonConfig,
    Pow2RangeChip, nb_foreign_ecc_chip_columns,
};
use midnight_circuits::hash::sha256::{
    NB_SHA256_ADVICE_COLS, NB_SHA256_FIXED_COLS, Sha256Chip, Sha256Config,
};

#[derive(Debug, Clone)]
pub struct IvcConfig {
    pub(crate) native_config: NativeConfig,
    pub(crate) core_decomp_config: P2RDecompositionConfig,
    pub(crate) jubjub_config: EccConfig,
    pub(crate) bls12_381_config: ForeignEccConfig<C>,
    pub(crate) poseidon_config: PoseidonConfig<F>,
    pub(crate) sha256_config: Sha256Config,
}

pub fn configure_ivc_circuit(meta: &mut ConstraintSystem<F>) -> IvcConfig {
    let nb_advice_cols = [
        NB_EDWARDS_COLS,
        NB_POSEIDON_ADVICE_COLS,
        NB_SHA256_ADVICE_COLS,
        nb_foreign_ecc_chip_columns::<F, C, C, NG>(),
    ]
    .into_iter()
    .max()
    .unwrap_or(0);

    let nb_fixed_cols = [
        NB_ARITH_FIXED_COLS,
        NB_POSEIDON_FIXED_COLS,
        NB_SHA256_FIXED_COLS,
    ]
    .into_iter()
    .max()
    .unwrap_or(0);

    let advice_columns: Vec<_> = (0..nb_advice_cols).map(|_| meta.advice_column()).collect();
    let fixed_columns: Vec<_> = (0..nb_fixed_cols).map(|_| meta.fixed_column()).collect();
    let committed_instance_column = meta.instance_column();
    let instance_column = meta.instance_column();

    let native_config = NativeChip::configure(
        meta,
        &(
            advice_columns[..NB_ARITH_COLS].try_into().unwrap(),
            fixed_columns[..NB_ARITH_FIXED_COLS].try_into().unwrap(),
            [committed_instance_column, instance_column],
        ),
    );
    let core_decomp_config = {
        let pow2_config = Pow2RangeChip::configure(meta, &advice_columns[1..NB_ARITH_COLS]);
        P2RDecompositionChip::configure(meta, &(native_config.clone(), pow2_config))
    };

    let jubjub_config =
        EccChip::<Jubjub>::configure(meta, &advice_columns[..NB_EDWARDS_COLS].try_into().unwrap());

    let base_config = FieldChip::<F, CBase, C, NG>::configure(meta, &advice_columns);
    let bls12_381_config =
        ForeignEccChip::<F, C, C, NG, NG>::configure(meta, &base_config, &advice_columns);

    let poseidon_config = PoseidonChip::configure(
        meta,
        &(
            advice_columns[..NB_POSEIDON_ADVICE_COLS]
                .try_into()
                .unwrap(),
            fixed_columns[..NB_POSEIDON_FIXED_COLS].try_into().unwrap(),
        ),
    );

    let sha256_config = Sha256Chip::configure(
        meta,
        &(
            advice_columns[..NB_SHA256_ADVICE_COLS].try_into().unwrap(),
            fixed_columns[..NB_SHA256_FIXED_COLS].try_into().unwrap(),
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
