use anyhow::{Context, anyhow};
use ff::Field;
use group::Group;
use midnight_circuits::ecc::curves::CircuitCurve as CircuitCurveTrait;
use midnight_circuits::instructions::{
    AssignmentInstructions, ConversionInstructions, PublicInputInstructions,
};
use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::circuit::{Layouter, Value};
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::{Relation, ZkStdLib, ZkStdLibArch};

use crate::circuits::halo2::errors::{StmCircuitError, to_synthesis_error};
use crate::circuits::halo2::gadgets::{
    verify_lottery, verify_merkle_path, verify_unique_signature,
};
use crate::circuits::halo2::types::{
    CircuitBase, CircuitCurve, LotteryIndex, MTLeaf, MerklePath, MerkleRoot,
    SignedMessageWithoutPrefix,
};
use crate::signature_scheme::{
    DOMAIN_SEPARATION_TAG_LOTTERY, DOMAIN_SEPARATION_TAG_SIGNATURE, PrimeOrderProjectivePoint,
    UniqueSchnorrSignature,
};
use crate::{Parameters, StmResult};

#[derive(Clone, Default, Debug)]
pub struct StmCircuit {
    // k in mithril: the required number of distinct lottery indices slots needed to create a valid multi-signature
    quorum: u32,
    // m in mithril: the number of lotteries that a user can participate in to sign a message
    num_lotteries: u32,
    merkle_tree_depth: u32,
}

impl StmCircuit {
    fn checked_len_u32(actual: usize) -> u32 {
        u32::try_from(actual).unwrap_or(u32::MAX)
    }

    /// Validates global circuit parameters before synthesis.
    ///
    /// Enforces `quorum < num_lotteries` returning
    /// `StmCircuitError::InvalidCircuitParameters` when violated.
    pub(crate) fn validate_parameters(&self) -> StmResult<()> {
        if self.quorum >= self.num_lotteries {
            return Err(anyhow!(StmCircuitError::InvalidCircuitParameters {
                quorum: self.quorum,
                num_lotteries: self.num_lotteries,
            }));
        }

        Ok(())
    }

    /// Validates that witness vector length matches the configured quorum.
    ///
    /// This precondition prevents shape mismatches; failures return
    /// `StmCircuitError::WitnessLengthMismatch`.
    pub(crate) fn validate_witness_length(&self, actual: usize) -> StmResult<()> {
        let expected_quorum = self.quorum as usize;
        if actual != expected_quorum {
            return Err(anyhow!(StmCircuitError::WitnessLengthMismatch {
                expected_quorum: self.quorum,
                actual: Self::checked_len_u32(actual),
            }));
        }

        Ok(())
    }

    /// Validates Merkle sibling path length against `merkle_tree_depth`.
    ///
    /// This guards against inconsistent witness paths and returns
    /// `StmCircuitError::MerkleSiblingLengthMismatch` on mismatch.
    pub(crate) fn validate_merkle_sibling_length(&self, actual: usize) -> StmResult<()> {
        let expected_depth = self.merkle_tree_depth as usize;
        if actual != expected_depth {
            return Err(anyhow!(StmCircuitError::MerkleSiblingLengthMismatch {
                expected_depth: self.merkle_tree_depth,
                actual: Self::checked_len_u32(actual),
            }));
        }

        Ok(())
    }

    /// Validates Merkle position bits length against `merkle_tree_depth`.
    ///
    /// Under the current witness shape, this cannot fail independently from sibling-length
    /// validation because both lengths derive from `x.siblings`; returns `StmCircuitError::MerklePositionLengthMismatch`.
    pub(crate) fn validate_merkle_position_length(&self, actual: usize) -> StmResult<()> {
        let expected_depth = self.merkle_tree_depth as usize;
        if actual != expected_depth {
            return Err(anyhow!(StmCircuitError::MerklePositionLengthMismatch {
                expected_depth: self.merkle_tree_depth,
                actual: Self::checked_len_u32(actual),
            }));
        }

        Ok(())
    }

    /// Constructs a new `StmCircuit` from Mithril `Parameters`.  
    ///  
    /// This constructor only validates that the quorum `k` and the number of lotteries `m`  
    /// fit into a `u32` by performing fallible `u64 -> u32` conversions. If either value  
    /// exceeds `u32::MAX`, it returns an error.  
    ///  
    /// It does **not** enforce semantic constraints such as `k < m` or other parameter  
    /// relationships; those invariants are validated later during circuit synthesis via  
    /// `validate_parameters`.
    pub fn try_new(stm_params: &Parameters, merkle_tree_depth: u32) -> StmResult<Self> {
        Ok(Self {
            quorum: stm_params
                .k
                .try_into()
                .with_context(|| format!(
                    "Failed to cast quorum as a u32. Its value ({}) is too large for the circuit.", 
                    stm_params.k)
                )?,
            num_lotteries: stm_params
                .m
                .try_into()
                .with_context(|| format!(
                    "Failed to cast number of lotteries as a u32. Its value ({}) is too large for the circuit.", 
                    stm_params.m),
                )?,
            merkle_tree_depth,
        })
    }
}

impl Relation for StmCircuit {
    type Instance = (MerkleRoot, SignedMessageWithoutPrefix);
    type Witness = Vec<(MTLeaf, MerklePath, UniqueSchnorrSignature, LotteryIndex)>;

    fn format_instance(instance: &Self::Instance) -> Result<Vec<CircuitBase>, Error> {
        Ok(vec![instance.0.into(), instance.1.into()])
    }

    fn circuit(
        &self,
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<CircuitBase>,
        instance: Value<Self::Instance>,
        witness: Value<Self::Witness>,
    ) -> Result<(), Error> {
        self.validate_parameters().map_err(to_synthesis_error)?;
        let witness = witness
            .map_with_result(|witness| -> StmResult<_> {
                self.validate_witness_length(witness.len())?;
                Ok(witness)
            })
            .map_err(to_synthesis_error)?
            .transpose_vec(self.quorum as usize);

        let merkle_root: AssignedNative<CircuitBase> =
            std_lib.assign_as_public_input(layouter, instance.map(|(x, _)| x.into()))?;
        let msg: AssignedNative<CircuitBase> =
            std_lib.assign_as_public_input(layouter, instance.map(|(_, x)| x.into()))?;

        // Compute H_1(merkle_root, msg)
        let hash = std_lib.hash_to_curve(layouter, &[merkle_root.clone(), msg.clone()])?;

        let generator: AssignedNativePoint<CircuitCurve> = std_lib.jubjub().assign_fixed(
            layouter,
            <CircuitCurve as CircuitCurveTrait>::CryptographicGroup::generator(),
        )?;

        let domain_separation_tag_signature: AssignedNative<_> =
            std_lib.assign_fixed(layouter, CircuitBase::from(DOMAIN_SEPARATION_TAG_SIGNATURE))?;
        let domain_separation_tag_lottery: AssignedNative<_> =
            std_lib.assign_fixed(layouter, CircuitBase::from(DOMAIN_SEPARATION_TAG_LOTTERY))?;
        let lottery_prefix = std_lib.poseidon(
            layouter,
            &[
                domain_separation_tag_lottery.clone(),
                merkle_root.clone(),
                msg.clone(),
            ],
        )?;

        let mut pre_index: AssignedNative<_> =
            std_lib.assign(layouter, Value::known(CircuitBase::ZERO))?;
        for (i, wit) in witness.into_iter().enumerate() {
            let index: AssignedNative<CircuitBase> = std_lib.assign(
                layouter,
                wit.clone().map(|(_, _, _, i)| CircuitBase::from(i as u64)),
            )?;

            // Check index order
            if i > 0 {
                let is_less = std_lib.lower_than(layouter, &pre_index, &index, 32)?;
                std_lib.assert_true(layouter, &is_less)?;
            }

            pre_index = index.clone();

            let vk = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(x, _, _, _)| x.0.0.0))?;

            let target: AssignedNative<CircuitBase> =
                std_lib.assign(layouter, wit.clone().map(|(x, _, _, _)| x.1.into()))?;

            // Assign sibling Values.
            let assigned_merkle_siblings = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map_with_result(|(_, x, _, _)| -> StmResult<_> {
                        self.validate_merkle_sibling_length(x.siblings.len())?;
                        Ok(x.siblings.iter().map(|sibling| sibling.1.into()).collect::<Vec<_>>())
                    })
                    .map_err(to_synthesis_error)?
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;

            // Assign sibling Position.
            let assigned_merkle_positions = std_lib.assign_many(
                layouter,
                wit.clone()
                    .map_with_result(|(_, x, _, _)| -> StmResult<_> {
                        self.validate_merkle_position_length(x.siblings.len())?;
                        Ok(x.siblings
                            .iter()
                            .map(|sibling| CircuitBase::from(sibling.0))
                            .collect::<Vec<_>>())
                    })
                    .map_err(to_synthesis_error)?
                    .transpose_vec(self.merkle_tree_depth as usize)
                    .as_slice(),
            )?;

            // Assert merkle positions are binary values.
            let assigned_merkle_positions = assigned_merkle_positions
                .iter()
                .map(|pos| std_lib.convert(layouter, pos))
                .collect::<Result<Vec<AssignedBit<CircuitBase>>, Error>>()?;

            let sigma_value = wit
                .clone()
                .map_with_result(|(_, _, sig, _)| {
                    let (u, v) = sig.commitment_point.get_coordinates();
                    PrimeOrderProjectivePoint::from_coordinates(u, v).map(|point| point.0)
                })
                .map_err(to_synthesis_error)?;
            let sigma: AssignedNativePoint<_> = std_lib.jubjub().assign(layouter, sigma_value)?;
            let s: AssignedScalarOfNativeCurve<CircuitCurve> = std_lib
                .jubjub()
                .assign(layouter, wit.clone().map(|(_, _, sig, _)| sig.response.0))?;
            let c_native = std_lib.assign(
                layouter,
                wit.map(|(_, _, sig, _)| CircuitBase::from(sig.challenge)),
            )?;
            let c: AssignedScalarOfNativeCurve<CircuitCurve> =
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
                &domain_separation_tag_signature,
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
        let m = std_lib.assign_fixed(layouter, CircuitBase::from(self.num_lotteries as u64))?;
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

#[cfg(test)]
mod dst_alignment_tests {
    use crate::circuits::halo2::types::CircuitBase;
    use crate::signature_scheme::{
        BaseFieldElement, DOMAIN_SEPARATION_TAG_LOTTERY, DOMAIN_SEPARATION_TAG_SIGNATURE,
        compute_poseidon_digest,
    };

    use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};

    const REFERENCE_SIGNATURE_DOMAIN_TAG: BaseFieldElement =
        BaseFieldElement(CircuitBase::from_raw([0x5349_474E_5F44_5354, 0, 0, 0]));
    const REFERENCE_LOTTERY_DOMAIN_TAG: BaseFieldElement =
        BaseFieldElement(CircuitBase::from_raw([0x4C4F_5454_5F44_5354, 0, 0, 0]));

    #[test]
    fn signature_and_lottery_domain_tags_do_not_collide() {
        assert_ne!(
            DOMAIN_SEPARATION_TAG_SIGNATURE, DOMAIN_SEPARATION_TAG_LOTTERY,
            "signature and lottery domain separation tags must be distinct"
        );
    }

    #[test]
    fn signature_digest_matches_reference_signature_domain_tag_formula() {
        let signature_transcript_inputs = vec![
            BaseFieldElement::from(11),
            BaseFieldElement::from(22),
            BaseFieldElement::from(33),
            BaseFieldElement::from(44),
        ];

        let mut stm_inputs = vec![DOMAIN_SEPARATION_TAG_SIGNATURE];
        stm_inputs.extend_from_slice(&signature_transcript_inputs);
        let signature_digest_via_stm = compute_poseidon_digest(&stm_inputs);

        let mut signature_digest_manual_inputs =
            vec![CircuitBase::from(REFERENCE_SIGNATURE_DOMAIN_TAG)];
        signature_digest_manual_inputs.extend(
            signature_transcript_inputs
                .iter()
                .map(|value| CircuitBase::from(*value)),
        );

        let signature_digest_via_reference_formula = BaseFieldElement(
            PoseidonChip::<CircuitBase>::hash(&signature_digest_manual_inputs),
        );

        assert_eq!(
            signature_digest_via_stm, signature_digest_via_reference_formula,
            "signature digest computed via STM helper must match reference signature-domain formula"
        );
    }

    #[test]
    fn lottery_prefix_matches_reference_lottery_domain_tag_formula() {
        let merkle_root = CircuitBase::from(123u64);
        let msg = CircuitBase::from(456u64);

        let lottery_prefix_via_stm_constant = PoseidonChip::<CircuitBase>::hash(&[
            CircuitBase::from(DOMAIN_SEPARATION_TAG_LOTTERY),
            merkle_root,
            msg,
        ]);

        let lottery_prefix_via_reference_formula = PoseidonChip::<CircuitBase>::hash(&[
            CircuitBase::from(REFERENCE_LOTTERY_DOMAIN_TAG),
            merkle_root,
            msg,
        ]);

        assert_eq!(
            BaseFieldElement(lottery_prefix_via_stm_constant),
            BaseFieldElement(lottery_prefix_via_reference_formula),
            "lottery prefix must use the lottery domain separation tag as first Poseidon input"
        );
    }
}

#[cfg(test)]
mod circuit_creation_tests {
    use crate::circuits::halo2::circuit::StmCircuit;

    #[test]
    fn correct_circuit_creation() {
        let stm_params = crate::Parameters {
            m: 100,
            k: 10,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 13;

        StmCircuit::try_new(&stm_params, merkle_tree_depth).unwrap();
    }

    #[test]
    fn circuit_creation_large_num_lotteries() {
        let stm_params = crate::Parameters {
            m: u32::MAX as u64 + 1,
            k: 10,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 13;

        let circuit = StmCircuit::try_new(&stm_params, merkle_tree_depth);

        circuit.expect_err("Creation should have failed with number of lotteries too large.");
    }

    #[test]
    fn circuit_creation_large_quorum() {
        let stm_params = crate::Parameters {
            m: 100,
            k: u32::MAX as u64 + 1,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 13;

        let circuit = StmCircuit::try_new(&stm_params, merkle_tree_depth);

        circuit.expect_err("Creation should have failed with quorum too large.");
    }
}
