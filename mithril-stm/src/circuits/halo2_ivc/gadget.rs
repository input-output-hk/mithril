use ff::Field;
use group::Group;
use midnight_circuits::hash::sha256::Sha256Chip;

use super::{
    Accumulator, ArithInstructions, AssertionInstructions, AssignedAccumulator, AssignedBit,
    AssignedForeignPoint, AssignedNative, AssignmentInstructions, BinaryInstructions, CircuitCurve,
    CircuitValue, ComposableChip, ControlFlowInstructions, EccChip, EccInstructions, EmulatedCurve,
    EqualityInstructions, Error, ForeignEccChip, HashInstructions, IvcNativeGadget, Layouter,
    NativeChip, NativeField, NativeGadget, P2RDecompositionChip, PREIMAGE_CURRENT_EPOCH_BYTES,
    PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES, PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES,
    PoseidonChip, PublicInputInstructions, RECURSIVE_CIRCUIT_DEGREE, RecursiveEmulation,
    VerifierGadget, ZeroInstructions,
    config::IvcConfig,
    gadgets::{GenesisSchnorrSignatureInputs, verify_genesis_signature},
    state::{AssignedGlobal, AssignedState, AssignedWitness},
};

#[derive(Debug, Clone)]
pub struct IvcGadget {
    pub(crate) core_decomp_chip: P2RDecompositionChip<NativeField>,
    pub(crate) native_gadget: IvcNativeGadget,
    pub(crate) jubjub_chip: EccChip<CircuitCurve>,
    pub(crate) poseidon_chip: PoseidonChip<NativeField>,
    pub(crate) sha2_256_chip: Sha256Chip<NativeField>,
    pub(crate) bls12_381_chip:
        ForeignEccChip<NativeField, EmulatedCurve, EmulatedCurve, IvcNativeGadget, IvcNativeGadget>,
    pub(crate) verifier_gadget: VerifierGadget<RecursiveEmulation>,
}

impl IvcGadget {
    pub fn new(config: &IvcConfig) -> Self {
        let native_chip = <NativeChip<NativeField> as ComposableChip<NativeField>>::new(
            &config.native_config,
            &(),
        );
        let core_decomp_chip = P2RDecompositionChip::new(
            &config.core_decomp_config,
            &(RECURSIVE_CIRCUIT_DEGREE as usize - 1),
        );
        let native_gadget = NativeGadget::new(core_decomp_chip.clone(), native_chip.clone());
        let jubjub_chip = EccChip::<CircuitCurve>::new(&config.jubjub_config, &native_gadget);
        let bls12_381_chip: ForeignEccChip<_, EmulatedCurve, EmulatedCurve, _, _> =
            { ForeignEccChip::new(&config.bls12_381_config, &native_gadget, &native_gadget) };
        let poseidon_chip = PoseidonChip::new(&config.poseidon_config, &native_chip);
        let sha2_256_chip = Sha256Chip::new(&config.sha256_config, &native_gadget);
        let verifier_gadget: VerifierGadget<RecursiveEmulation> =
            VerifierGadget::new(&bls12_381_chip, &native_gadget, &poseidon_chip);

        IvcGadget {
            core_decomp_chip,
            native_gadget,
            jubjub_chip,
            poseidon_chip,
            sha2_256_chip,
            bls12_381_chip,
            verifier_gadget,
        }
    }

    pub fn is_genesis(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        state: &AssignedState,
    ) -> Result<AssignedBit<NativeField>, Error> {
        self.native_gadget.is_zero(layouter, &state.step_counter)
    }

    /// Verifies the genesis Schnorr signature in-circuit (delegates to the `schnorr_signature` gadget).
    pub fn is_genesis_signature_valid(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        global: &AssignedGlobal,
        witness: &AssignedWitness,
    ) -> Result<AssignedBit<NativeField>, Error> {
        verify_genesis_signature(
            &self.jubjub_chip,
            &self.native_gadget,
            &self.poseidon_chip,
            layouter,
            GenesisSchnorrSignatureInputs {
                verification_key: &global.genesis_verification_key,
                message: &global.genesis_message,
                response: &witness.genesis_signature.0,
                challenge: &witness.genesis_signature.1,
            },
        )
    }

    pub fn assert_genesis(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        is_not_genesis: &AssignedBit<NativeField>,
        global: &AssignedGlobal,
        witness: &AssignedWitness,
    ) -> Result<(), Error> {
        // Verify the genesis signature
        let is_genesis_signature_valid =
            self.is_genesis_signature_valid(layouter, global, witness)?;

        // Skip the genesis signature verification if it is not genesis
        let check_genesis = self.native_gadget.or(
            layouter,
            &[is_genesis_signature_valid, is_not_genesis.clone()],
        )?;
        self.native_gadget
            .assert_equal_to_fixed(layouter, &check_genesis, true)
    }

    pub fn global_as_public_input(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        global: &AssignedGlobal,
    ) -> Result<Vec<AssignedNative<NativeField>>, Error> {
        let pi = [
            vec![
                global.genesis_message.clone(),
                self.jubjub_chip.x_coordinate(&global.genesis_verification_key),
                self.jubjub_chip.y_coordinate(&global.genesis_verification_key),
            ],
            self.verifier_gadget
                .as_public_input(layouter, &global.certificate_verification_key)?,
            self.verifier_gadget
                .as_public_input(layouter, &global.ivc_verification_key)?,
        ]
        .concat();
        Ok(pi)
    }

    fn combine_bytes(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        bytes: impl IntoIterator<Item = impl Into<AssignedNative<NativeField>>>,
        bases: &[NativeField],
    ) -> Result<AssignedNative<NativeField>, Error> {
        let items: Vec<_> = bytes
            .into_iter()
            .zip(bases.iter())
            .map(|(v, base)| (*base, v.into()))
            .collect();

        self.native_gadget
            .linear_combination(layouter, &items, NativeField::ZERO)
    }

    pub fn transition(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        is_genesis: &AssignedBit<NativeField>,
        is_not_genesis: &AssignedBit<NativeField>,
        global: &AssignedGlobal,
        state: &AssignedState,
        witness: &AssignedWitness,
    ) -> Result<AssignedState, Error> {
        let step_counter =
            self.native_gadget
                .add_constant(layouter, &state.step_counter, NativeField::ONE)?;

        let (certificate_message, certificate_merkle_tree_commitment) = (
            witness.certificate_message.clone(),
            witness.certificate_merkle_tree_commitment.clone(),
        );

        // Open message hash to check the link between certificates
        // If it is genesis, select genesis message as message; otherwise, select cert message as message.
        let message = self.native_gadget.select(
            layouter,
            is_genesis,
            &global.genesis_message,
            &certificate_message,
        )?;

        let hash = self.sha2_256_chip.hash(layouter, &witness.message_preimage)?;

        let factor = NativeField::from(256u64);
        let bases: Vec<_> = (0..32)
            .scan(NativeField::ONE, |s, _| {
                let out = *s;
                *s *= factor;
                Some(out)
            })
            .collect();

        {
            // Compare message and hash
            let hash_native = self.combine_bytes(layouter, &hash, &bases)?;
            self.native_gadget.assert_equal(layouter, &message, &hash_native)?;
        }

        // If it is genesis, merkle_tree_commitment = 0; otherwise, merkle_tree_commitment = certificate_merkle_tree_commitment.
        let zero = self.native_gadget.assign_fixed(layouter, NativeField::ZERO)?;
        let merkle_tree_commitment = self.native_gadget.select(
            layouter,
            is_genesis,
            &zero,
            &certificate_merkle_tree_commitment,
        )?;

        // Read the next Merkle-tree commitment, next protocol parameters, and current epoch from the protocol message preimage.
        // digest(6) | bytes(32) | next_aggregate_verification_key(31) | bytes(44) | next_protocol_parameters(24) | bytes(32) | current_epoch(13) | bytes(8)
        // todo: check field keywords(?)
        let next_merkle_tree_commitment_bytes =
            witness.message_preimage[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES].to_vec();
        let next_protocol_parameters_bytes =
            witness.message_preimage[PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES].to_vec();
        let current_epoch_bytes = witness.message_preimage[PREIMAGE_CURRENT_EPOCH_BYTES].to_vec();

        // Get the field elements by linearly combining the bytes
        let (next_merkle_tree_commitment, next_protocol_parameters, current_epoch) = {
            let next_merkle_tree_commitment =
                self.combine_bytes(layouter, next_merkle_tree_commitment_bytes, &bases)?;
            let next_protocol_parameters =
                self.combine_bytes(layouter, next_protocol_parameters_bytes, &bases)?;
            let current_epoch = self.combine_bytes(layouter, current_epoch_bytes, &bases)?;
            (
                next_merkle_tree_commitment,
                next_protocol_parameters,
                current_epoch,
            )
        };

        let (is_same_epoch, is_next_epoch) = {
            // current_epoch == state.current_epoch
            let is_same_epoch =
                self.native_gadget
                    .is_equal(layouter, &current_epoch, &state.current_epoch)?;

            //  current_epoch == state.current_epoch + 1
            let next = self.native_gadget.add_constant(
                layouter,
                &state.current_epoch,
                NativeField::ONE,
            )?;
            let is_next_epoch = self.native_gadget.is_equal(layouter, &current_epoch, &next)?;

            (is_same_epoch, is_next_epoch)
        };

        {
            // If state.step_counter == 1, the previous certificate is a genesis certificate and
            // the current certificate is the first certificate after the genesis and
            // its epoch number must be the next epoch number.
            // Assert true: is_not_first or is_next_epoch
            let is_first = self.native_gadget.is_equal_to_fixed(
                layouter,
                &state.step_counter,
                NativeField::ONE,
            )?;
            let is_not_first = self.native_gadget.not(layouter, &is_first)?;

            let is_valid = self
                .native_gadget
                .or(layouter, &[is_not_first, is_next_epoch.clone()])?;
            self.native_gadget.assert_equal_to_fixed(layouter, &is_valid, true)?;
        }

        {
            // Check the current Merkle-tree commitment link; if it is genesis, skip the check.
            // Assert true: is_genesis or (is_same_epoch && merkle_tree_commitment == state.merkle_tree_commitment) or (is_next_epoch && merkle_tree_commitment == state.next_merkle_tree_commitment)
            let mut is_equal_current = self.native_gadget.is_equal(
                layouter,
                &merkle_tree_commitment,
                &state.merkle_tree_commitment,
            )?;
            is_equal_current = self
                .native_gadget
                .and(layouter, &[is_equal_current, is_same_epoch.clone()])?;

            let mut is_equal_next = self.native_gadget.is_equal(
                layouter,
                &merkle_tree_commitment,
                &state.next_merkle_tree_commitment,
            )?;
            is_equal_next = self
                .native_gadget
                .and(layouter, &[is_equal_next, is_next_epoch.clone()])?;

            let is_link_valid = self.native_gadget.or(
                layouter,
                &[is_genesis.clone(), is_equal_current, is_equal_next],
            )?;
            self.native_gadget
                .assert_equal_to_fixed(layouter, &is_link_valid, true)?;
        }

        let protocol_parameters = {
            // If genesis: protocol_parameters = 0
            // Else:
            //     if same_epoch: protocol_parameters = state.protocol_parameters;
            //     else: protocol_parameters = state.next_protocol_parameters
            let mut protocol_parameters = self.native_gadget.select(
                layouter,
                is_genesis,
                &zero,
                &state.next_protocol_parameters,
            )?;
            let is_same_epoch_not_genesis = self
                .native_gadget
                .and(layouter, &[is_same_epoch.clone(), is_not_genesis.clone()])?;
            protocol_parameters = self.native_gadget.select(
                layouter,
                &is_same_epoch_not_genesis,
                &state.protocol_parameters,
                &protocol_parameters,
            )?;
            protocol_parameters
        };

        {
            // Check the consistency on next_merkle_tree_commitment and next_protocol_parameters for certificates of the same epoch
            // Assert true: is_genesis or (is_same_epoch && next_merkle_tree_commitment == state.next_merkle_tree_commitment && next_protocol_parameters == state.next_protocol_parameters) or is_next_epoch
            let is_equal_mt = self.native_gadget.is_equal(
                layouter,
                &next_merkle_tree_commitment,
                &state.next_merkle_tree_commitment,
            )?;
            let is_equal_pp = self.native_gadget.is_equal(
                layouter,
                &next_protocol_parameters,
                &state.next_protocol_parameters,
            )?;
            let mut is_valid = self
                .native_gadget
                .and(layouter, &[is_same_epoch, is_equal_mt, is_equal_pp])?;
            is_valid = self
                .native_gadget
                .or(layouter, &[is_genesis.clone(), is_valid, is_next_epoch])?;
            self.native_gadget.assert_equal_to_fixed(layouter, &is_valid, true)?;
        };

        // Return the next state
        Ok(AssignedState {
            step_counter,
            message,
            merkle_tree_commitment,
            next_merkle_tree_commitment,
            protocol_parameters,
            next_protocol_parameters,
            current_epoch,
        })
    }

    #[allow(clippy::too_many_arguments)]
    pub fn verify_prepare(
        &self,
        layouter: &mut impl Layouter<NativeField>,
        global: &AssignedGlobal,
        is_not_genesis: &AssignedBit<NativeField>,
        state: &AssignedState,
        witness: &AssignedWitness,
        certificate_proof: &CircuitValue<Vec<u8>>,
        ivc_proof: &CircuitValue<Vec<u8>>,
        acc_value: &CircuitValue<Accumulator<RecursiveEmulation>>,
    ) -> Result<AssignedAccumulator<RecursiveEmulation>, Error> {
        let id_point: AssignedForeignPoint<_, _, _> = self
            .bls12_381_chip
            .assign_fixed(layouter, EmulatedCurve::identity())?;

        let mut certificate_proof_accumulator = self.verifier_gadget.prepare(
            layouter,
            &global.certificate_verification_key,
            &[("com_instance", id_point.clone())],
            &[&[
                witness.certificate_merkle_tree_commitment.clone(),
                witness.certificate_message.clone(),
            ]],
            certificate_proof.clone(),
        )?;

        // If it is genesis, we allow the prover to change the (probably
        // invalid) accumulator by a default accumulator that satisfies the invariant.
        AssignedAccumulator::scale_by_bit(
            layouter,
            &self.native_gadget,
            is_not_genesis,
            &mut certificate_proof_accumulator,
        )?;
        certificate_proof_accumulator.collapse(
            layouter,
            &self.bls12_381_chip,
            &self.native_gadget,
        )?;

        let acc = AssignedAccumulator::assign(
            layouter,
            &self.bls12_381_chip,
            &self.native_gadget,
            1,
            1,
            &[],
            &global.fixed_base_names,
            acc_value.clone(),
        )?;

        // Public inputs for this IVC circuit:
        // [global, state, acc]
        let assigned_pi = [
            self.global_as_public_input(layouter, global)?,
            state.as_public_input(),
            self.verifier_gadget.as_public_input(layouter, &acc)?,
        ]
        .concat();

        // Verify a witnessed proof that ensures the validity of the previous state.
        // The proof is valid iff `ivc_proof_accumulator` satisfies the invariant.
        let mut ivc_proof_accumulator = self.verifier_gadget.prepare(
            layouter,
            &global.ivc_verification_key,
            &[("com_instance", id_point)],
            &[&assigned_pi],
            ivc_proof.clone(),
        )?;

        // If the previous state is genesis, we allow the prover to change the (probably
        // invalid) accumulator by a default accumulator that satisfies the invariant.
        AssignedAccumulator::scale_by_bit(
            layouter,
            &self.native_gadget,
            is_not_genesis,
            &mut ivc_proof_accumulator,
        )?;
        ivc_proof_accumulator.collapse(layouter, &self.bls12_381_chip, &self.native_gadget)?;

        // Accumulate the certificate and IVC proof accumulators.
        let mut next_acc = AssignedAccumulator::<RecursiveEmulation>::accumulate(
            layouter,
            &self.verifier_gadget,
            &self.native_gadget,
            &self.poseidon_chip,
            &[acc, certificate_proof_accumulator, ivc_proof_accumulator],
        )?;
        next_acc.collapse(layouter, &self.bls12_381_chip, &self.native_gadget)?;

        Ok(next_acc)
    }
}
