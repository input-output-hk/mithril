use ff::Field;
use group::Group;
use midnight_circuits::ecc::curves::CircuitCurve;
use midnight_circuits::instructions::{
    AssignmentInstructions, ConversionInstructions, PublicInputInstructions,
};
use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::circuit::{Layouter, Value};
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::{Relation, ZkStdLib, ZkStdLibArch};

use crate::circuits::halo2::constants::{DST_LOTTERY, DST_UNIQUE_SIGNATURE};
use crate::circuits::halo2::gadgets::{
    verify_lottery, verify_merkle_path, verify_unique_signature,
};
use crate::circuits::halo2::off_circuit::merkle_tree::{MTLeaf, MerklePath};
use crate::circuits::halo2::off_circuit::unique_signature::Signature;
use crate::circuits::halo2::types::{Jubjub, JubjubBase, LotteryIndex, MerkleRoot, Msg};

type F = JubjubBase;
type C = Jubjub;

#[derive(Clone, Default, Debug)]
pub struct StmCircuit {
    // k in mithril: the required number of distinct lottery indices slots needed to create a valid multi-signature
    quorum: u32,
    // m in mithril: the number of lotteries that a user can participate in to sign a message
    num_lotteries: u32,
    merkle_tree_depth: u32,
}

impl StmCircuit {
    pub fn new(quorum: u32, num_lotteries: u32, merkle_tree_depth: u32) -> Self {
        Self {
            quorum,
            num_lotteries,
            merkle_tree_depth,
        }
    }
}

impl Relation for StmCircuit {
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

        // Compute H_1(merkle_root, msg)
        let hash = std_lib.hash_to_curve(layouter, &[merkle_root.clone(), msg.clone()])?;

        let generator: AssignedNativePoint<C> = std_lib.jubjub().assign_fixed(
            layouter,
            <C as CircuitCurve>::CryptographicGroup::generator(),
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

            // Check index order
            if i > 0 {
                let is_less = std_lib.lower_than(layouter, &pre_index, &index, 32)?;
                std_lib.assert_true(layouter, &is_less)?;
            }

            pre_index = index.clone();

            let vk = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(x, _, _, _)| x.0.0))?;

            let target: AssignedNative<F> =
                std_lib.assign(layouter, wit.clone().map(|(x, _, _, _)| x.1))?;

            // Assign sibling Values.
            let assigned_merkle_siblings = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map(|(_, x, _, _)| x.siblings.iter().map(|x| x.1).collect::<Vec<_>>())
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;

            // Assign sibling Position.
            let assigned_merkle_positions = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map(|(_, x, _, _)| x.siblings.iter().map(|x| x.0.into()).collect::<Vec<_>>())
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;

            // Assert merkle positions are binary values.
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

        // m can be put as a public instance or a constant
        let m = std_lib.assign_fixed(layouter, F::from(self.num_lotteries as u64))?;
        let is_less = std_lib.lower_than(layouter, &pre_index, &m, 32)?;

        std_lib.assert_true(layouter, &is_less)
    }

    fn used_chips(&self) -> ZkStdLibArch {
        ZkStdLibArch {
            jubjub: true,
            poseidon: true,
            sha2_256: false,
            sha2_512: false,
            keccak_256: false,
            sha3_256: false,
            secp256k1: false,
            bls12_381: false,
            base64: false,
            nr_pow2range_cols: 2,
            automaton: false,
            blake2b: false,
        }
    }

    fn write_relation<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&self.quorum.to_le_bytes())?;
        writer.write_all(&self.num_lotteries.to_le_bytes())?;
        writer.write_all(&self.merkle_tree_depth.to_le_bytes())
    }

    fn read_relation<R: std::io::Read>(reader: &mut R) -> std::io::Result<Self> {
        // Buffers to read 4 bytes for each `u32` field.
        let mut quorum_bytes = [0u8; 4];
        let mut num_lotteries_bytes = [0u8; 4];
        let mut merkle_tree_depth_bytes = [0u8; 4];

        // Read the values into their corresponding buffers.
        reader.read_exact(&mut quorum_bytes)?;
        reader.read_exact(&mut num_lotteries_bytes)?;
        reader.read_exact(&mut merkle_tree_depth_bytes)?;

        // Convert the byte arrays back into `u32` values.
        let quorum = u32::from_le_bytes(quorum_bytes);
        let num_lotteries = u32::from_le_bytes(num_lotteries_bytes);
        let merkle_tree_depth = u32::from_le_bytes(merkle_tree_depth_bytes);

        // Construct and return the `StmCircuit` instance.
        Ok(Self {
            quorum,
            num_lotteries,
            merkle_tree_depth,
        })
    }
}
