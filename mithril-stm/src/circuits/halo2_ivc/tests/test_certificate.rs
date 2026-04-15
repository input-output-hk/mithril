use ff::{Field, PrimeField};
use group::Group;
use midnight_circuits::instructions::{
    ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
    ControlFlowInstructions, ConversionInstructions, DecompositionInstructions, EccInstructions,
    EqualityInstructions, PublicInputInstructions,
};
use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::{
    circuit::{Layouter, Value},
    plonk::Error,
};
use midnight_zk_stdlib::{Relation, ZkStdLib, ZkStdLibArch};

use super::super::helpers::{
    merkle_tree::{MTLeaf, MerklePath},
    signatures::unique_signature::Signature,
    utils::split,
};
use super::super::{DST_UNIQUE_SIGNATURE, Jubjub as CircuitJubjub, JubjubBase};

const DST_LOTTERY: JubjubBase = JubjubBase::from_raw([3, 3, 0, 0]);

type F = JubjubBase;
type C = CircuitJubjub;
type MerkleRoot = F;
type Msg = F;
type LotteryIndex = u32;

fn decompose_unsafe(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<F>,
    x: &AssignedNative<F>,
) -> Result<(AssignedNative<F>, AssignedNative<F>), Error> {
    let x_value = x.value();
    let base127 = F::from_u128(1_u128 << 127);
    let (x_low, x_high) = x_value.map(|v| split(v, 127)).unzip();

    let x_low_assigned: AssignedNative<_> = std_lib.assign(layouter, x_low.clone())?;
    let x_high_assigned: AssignedNative<_> = std_lib.assign(layouter, x_high.clone())?;

    let x_combined: AssignedNative<_> = std_lib.linear_combination(
        layouter,
        &[(F::ONE, x_low_assigned.clone()), (base127, x_high_assigned.clone())],
        F::ZERO,
    )?;
    std_lib.assert_equal(layouter, x, &x_combined)?;

    let sgn0 = std_lib.sgn0(layouter, x)?;
    let sgn1 = std_lib.sgn0(layouter, &x_low_assigned)?;
    std_lib.assert_equal(layouter, &sgn0, &sgn1)?;

    Ok((x_low_assigned, x_high_assigned))
}

fn lower_than_native(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<F>,
    x: &AssignedNative<F>,
    y: &AssignedNative<F>,
) -> Result<AssignedBit<F>, Error> {
    let (x_low_assigned, x_high_assigned) = decompose_unsafe(std_lib, layouter, x)?;
    let (y_low_assigned, y_high_assigned) = decompose_unsafe(std_lib, layouter, y)?;

    let is_equal_high = std_lib.is_equal(layouter, &x_high_assigned, &y_high_assigned)?;
    let is_less_low = std_lib.lower_than(layouter, &x_low_assigned, &y_low_assigned, 127)?;
    let is_less_high = std_lib.lower_than(layouter, &x_high_assigned, &y_high_assigned, 128)?;

    let low_less = std_lib.and(layouter, &[is_equal_high, is_less_low])?;
    std_lib.or(layouter, &[is_less_high, low_less])
}

fn verify_merkle_path(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<F>,
    vk: &AssignedNativePoint<C>,
    target: &AssignedNative<F>,
    merkle_root: &AssignedNative<F>,
    merkle_siblings: &[AssignedNative<F>],
    merkle_positions: &[AssignedBit<F>],
) -> Result<(), Error> {
    let vk_x = std_lib.jubjub().x_coordinate(vk);
    let vk_y = std_lib.jubjub().y_coordinate(vk);
    let leaf = std_lib.poseidon(layouter, &[vk_x.clone(), vk_y.clone(), target.clone()])?;
    let root =
        merkle_siblings
            .iter()
            .zip(merkle_positions.iter())
            .try_fold(leaf, |acc, (x, pos)| {
                let left = std_lib.select(layouter, pos, &acc, x)?;
                let right = std_lib.select(layouter, pos, x, &acc)?;
                std_lib.poseidon(layouter, &[left, right])
            })?;

    std_lib.assert_equal(layouter, &root, merkle_root)
}

fn verify_unique_signature(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<F>,
    dst_signature: &AssignedNative<F>,
    generator: &AssignedNativePoint<C>,
    vk: &AssignedNativePoint<C>,
    s: &AssignedScalarOfNativeCurve<C>,
    c: &AssignedScalarOfNativeCurve<C>,
    c_native: &AssignedNative<F>,
    hash: &AssignedNativePoint<C>,
    sigma: &AssignedNativePoint<C>,
) -> Result<(), Error> {
    let cap_r_1 = std_lib.jubjub().msm(
        layouter,
        &[s.clone(), c.clone()],
        &[hash.clone(), sigma.clone()],
    )?;
    let cap_r_2 = std_lib.jubjub().msm(
        layouter,
        &[s.clone(), c.clone()],
        &[generator.clone(), vk.clone()],
    )?;

    let hx = std_lib.jubjub().x_coordinate(hash);
    let hy = std_lib.jubjub().y_coordinate(hash);
    let vk_x = std_lib.jubjub().x_coordinate(vk);
    let vk_y = std_lib.jubjub().y_coordinate(vk);
    let sigma_x = std_lib.jubjub().x_coordinate(sigma);
    let sigma_y = std_lib.jubjub().y_coordinate(sigma);
    let cap_r_1_x = std_lib.jubjub().x_coordinate(&cap_r_1);
    let cap_r_1_y = std_lib.jubjub().y_coordinate(&cap_r_1);
    let cap_r_2_x = std_lib.jubjub().x_coordinate(&cap_r_2);
    let cap_r_2_y = std_lib.jubjub().y_coordinate(&cap_r_2);

    let c_prime = std_lib.poseidon(
        layouter,
        &[
            dst_signature.clone(),
            hx,
            hy,
            vk_x,
            vk_y,
            sigma_x,
            sigma_y,
            cap_r_1_x,
            cap_r_1_y,
            cap_r_2_x,
            cap_r_2_y,
        ],
    )?;
    std_lib.assert_equal(layouter, c_native, &c_prime)
}

fn verify_lottery(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<F>,
    lottery_prefix: &AssignedNative<F>,
    sigma: &AssignedNativePoint<C>,
    index: &AssignedNative<F>,
    target: &AssignedNative<F>,
) -> Result<(), Error> {
    let sigma_x = std_lib.jubjub().x_coordinate(sigma);
    let sigma_y = std_lib.jubjub().y_coordinate(sigma);
    let ev = std_lib.poseidon(
        layouter,
        &[lottery_prefix.clone(), sigma_x, sigma_y, index.clone()],
    )?;
    let is_less = lower_than_native(std_lib, layouter, target, &ev)?;
    std_lib.assert_false(layouter, &is_less)
}

#[derive(Clone, Default, Debug)]
pub(crate) struct Certificate {
    quorum: u32,
    num_lotteries: u32,
    merkle_tree_depth: u32,
}

impl Certificate {
    pub(crate) fn new(quorum: u32, num_lotteries: u32, merkle_tree_depth: u32) -> Self {
        Self {
            quorum,
            num_lotteries,
            merkle_tree_depth,
        }
    }
}

impl Relation for Certificate {
    type Instance = (MerkleRoot, Msg);
    type Witness = Vec<(MTLeaf, MerklePath, Signature, LotteryIndex)>;

    fn format_instance(instance: &Self::Instance) -> Result<Vec<F>, Error> {
        Ok(vec![instance.0, instance.1])
    }

    fn circuit(
        &self,
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<F>,
        instance: Value<Self::Instance>,
        witness: Value<Self::Witness>,
    ) -> Result<(), Error> {
        assert!(self.quorum < self.num_lotteries);

        let merkle_root: AssignedNative<F> =
            std_lib.assign_as_public_input(layouter, instance.map(|(x, _)| x))?;
        let msg: AssignedNative<F> =
            std_lib.assign_as_public_input(layouter, instance.map(|(_, x)| x))?;

        let hash = std_lib.hash_to_curve(layouter, &[merkle_root.clone(), msg.clone()])?;
        let generator: AssignedNativePoint<C> = std_lib.jubjub().assign_fixed(
            layouter,
            <C as midnight_circuits::ecc::curves::CircuitCurve>::CryptographicGroup::generator(),
        )?;

        let dst_signature: AssignedNative<_> =
            std_lib.assign_fixed(layouter, DST_UNIQUE_SIGNATURE)?;
        let dst_lottery: AssignedNative<_> = std_lib.assign_fixed(layouter, DST_LOTTERY)?;
        let lottery_prefix = std_lib.poseidon(
            layouter,
            &[dst_lottery.clone(), merkle_root.clone(), msg.clone()],
        )?;

        let witness = witness.transpose_vec(self.quorum as usize);
        let mut pre_index: AssignedNative<_> = std_lib.assign(layouter, Value::known(F::ZERO))?;
        for (i, wit) in witness.into_iter().enumerate() {
            let index: AssignedNative<F> =
                std_lib.assign(layouter, wit.clone().map(|(_, _, _, i)| F::from(i as u64)))?;

            if i > 0 {
                let is_less = std_lib.lower_than(layouter, &pre_index, &index, 16)?;
                std_lib.assert_true(layouter, &is_less)?;
            }

            pre_index = index.clone();

            let vk = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(x, _, _, _)| x.0.0))?;
            let target: AssignedNative<F> =
                std_lib.assign(layouter, wit.clone().map(|(x, _, _, _)| x.1))?;

            let assigned_merkle_siblings = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map(|(_, x, _, _)| x.siblings.iter().map(|x| x.1).collect::<Vec<_>>())
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;
            let assigned_merkle_positions = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map(|(_, x, _, _)| x.siblings.iter().map(|x| x.0.into()).collect::<Vec<_>>())
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;
            let assigned_merkle_positions = assigned_merkle_positions
                .iter()
                .map(|pos| std_lib.convert(layouter, pos))
                .collect::<Result<Vec<AssignedBit<F>>, Error>>()?;

            let sigma: AssignedNativePoint<_> = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(_, _, sig, _)| sig.sigma))?;
            let s: AssignedScalarOfNativeCurve<C> = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(_, _, sig, _)| sig.s))?;
            let c_native = std_lib.assign(layouter, wit.map(|(_, _, sig, _)| sig.c))?;
            let c: AssignedScalarOfNativeCurve<C> =
                std_lib.jubjub().convert(layouter, &c_native)?;

            verify_merkle_path(
                std_lib,
                layouter,
                &vk,
                &target,
                &merkle_root,
                &assigned_merkle_siblings,
                &assigned_merkle_positions,
            )?;
            verify_unique_signature(
                std_lib,
                layouter,
                &dst_signature,
                &generator,
                &vk,
                &s,
                &c,
                &c_native,
                &hash,
                &sigma,
            )?;
            verify_lottery(std_lib, layouter, &lottery_prefix, &sigma, &index, &target)?;
        }

        let m = std_lib.assign_fixed(layouter, F::from(self.num_lotteries as u64))?;
        let is_less = std_lib.lower_than(layouter, &pre_index, &m, 16)?;
        std_lib.assert_true(layouter, &is_less)
    }

    fn used_chips(&self) -> ZkStdLibArch {
        ZkStdLibArch {
            jubjub: true,
            poseidon: true,
            sha2_256: false,
            sha2_512: false,
            sha3_256: false,
            keccak_256: false,
            blake2b: false,
            secp256k1: false,
            bls12_381: false,
            base64: false,
            nr_pow2range_cols: 2,
            automaton: false,
        }
    }

    fn write_relation<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&self.quorum.to_le_bytes())?;
        writer.write_all(&self.num_lotteries.to_le_bytes())?;
        writer.write_all(&self.merkle_tree_depth.to_le_bytes())
    }

    fn read_relation<R: std::io::Read>(reader: &mut R) -> std::io::Result<Self> {
        let mut quorum_bytes = [0u8; 4];
        let mut num_lotteries_bytes = [0u8; 4];
        let mut merkle_tree_depth_bytes = [0u8; 4];
        reader.read_exact(&mut quorum_bytes)?;
        reader.read_exact(&mut num_lotteries_bytes)?;
        reader.read_exact(&mut merkle_tree_depth_bytes)?;

        Ok(Self {
            quorum: u32::from_le_bytes(quorum_bytes),
            num_lotteries: u32::from_le_bytes(num_lotteries_bytes),
            merkle_tree_depth: u32::from_le_bytes(merkle_tree_depth_bytes),
        })
    }
}
