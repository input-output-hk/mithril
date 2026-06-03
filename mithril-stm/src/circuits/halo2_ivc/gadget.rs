use std::collections::HashSet;

use ff::Field;
use group::Group;
use midnight_circuits::hash::sha256::Sha256Chip;

use crate::signature_scheme::DOMAIN_SEPARATION_TAG_STANDARD_SIGNATURE;

use super::{
    Accumulator, ArithInstructions, AssertionInstructions, AssignedAccumulator, AssignedBit,
    AssignedForeignPoint, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
    AssignedVk, AssignmentInstructions, BinaryInstructions, C, CERTIFICATE_VERIFICATION_KEY_NAME,
    CircuitCurve, ComposableChip, ConstraintSystem, ControlFlowInstructions,
    ConversionInstructions, EccChip, EccInstructions, EqualityInstructions, Error,
    EvaluationDomain, F, ForeignEccChip, HashInstructions, IVC_VERIFICATION_KEY_NAME, Jubjub, K,
    Layouter, NG, NativeChip, NativeGadget, P2RDecompositionChip, PREIMAGE_CURRENT_EPOCH_BYTES,
    PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES, PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES,
    PoseidonChip, PublicInputInstructions, S, Value, VerifierGadget, ZeroInstructions,
    config::IvcConfig,
    errors::{IvcCircuitError, to_synthesis_error},
    state::{
        AssignedGlobal, AssignedState, AssignedWitness, Global, State, Witness, fixed_base_names,
    },
};

#[derive(Debug, Clone)]
pub struct IvcGadget {
    pub(crate) core_decomp_chip: P2RDecompositionChip<F>,
    pub(crate) native_gadget: NG,
    pub(crate) jubjub_chip: EccChip<Jubjub>,
    pub(crate) poseidon_chip: PoseidonChip<F>,
    pub(crate) sha2_256_chip: Sha256Chip<F>,
    pub(crate) bls12_381_chip: ForeignEccChip<F, C, C, NG, NG>,
    pub(crate) verifier_gadget: VerifierGadget<S>,
}

impl IvcGadget {
    pub fn new(config: &IvcConfig) -> Self {
        let native_chip = <NativeChip<F> as ComposableChip<F>>::new(&config.native_config, &());
        let core_decomp_chip =
            P2RDecompositionChip::new(&config.core_decomp_config, &(K as usize - 1));
        let native_gadget = NativeGadget::new(core_decomp_chip.clone(), native_chip.clone());
        let jubjub_chip = EccChip::<Jubjub>::new(&config.jubjub_config, &native_gadget);
        let bls12_381_chip: ForeignEccChip<_, C, C, _, _> =
            { ForeignEccChip::new(&config.bls12_381_config, &native_gadget, &native_gadget) };
        let poseidon_chip = PoseidonChip::new(&config.poseidon_config, &native_chip);
        let sha2_256_chip = Sha256Chip::new(&config.sha256_config, &native_gadget);
        let verifier_gadget: VerifierGadget<S> =
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

    pub fn assign_global_as_public_input(
        &self,
        layouter: &mut impl Layouter<F>,
        global: &Value<Global>,
        certificate_circuit_domain_and_constraint_system: &(
            EvaluationDomain<F>,
            ConstraintSystem<F>,
        ),
        ivc_circuit_domain_and_constraint_system: &(EvaluationDomain<F>, ConstraintSystem<F>),
    ) -> Result<AssignedGlobal, Error> {
        let genesis_message: AssignedNative<_> = self.native_gadget.assign_as_public_input(
            layouter,
            global.clone().map(|gl| gl.genesis_message.as_field()),
        )?;
        let genesis_verification_key: AssignedNativePoint<_> =
            self.jubjub_chip.assign_as_public_input(
                layouter,
                global
                    .clone()
                    .map(|gl| *gl.genesis_verification_key.as_jubjub_subgroup()),
            )?;

        let (certificate_circuit_domain, certificate_circuit_constraint_system) =
            &certificate_circuit_domain_and_constraint_system;
        let certificate_verification_key: AssignedVk<S> =
            self.verifier_gadget.assign_vk_as_public_input(
                layouter,
                CERTIFICATE_VERIFICATION_KEY_NAME,
                certificate_circuit_domain,
                certificate_circuit_constraint_system,
                global
                    .clone()
                    .map(|gl| gl.certificate_circuit_verification_key_representation.as_field()),
            )?;

        // Assign for IVC proof verification
        let (ivc_circuit_domain, ivc_circuit_constraint_system) =
            &ivc_circuit_domain_and_constraint_system;
        let ivc_verification_key: AssignedVk<S> = self.verifier_gadget.assign_vk_as_public_input(
            layouter,
            IVC_VERIFICATION_KEY_NAME,
            ivc_circuit_domain,
            ivc_circuit_constraint_system,
            global
                .clone()
                .map(|gl| gl.ivc_circuit_verification_key_representation.as_field()),
        )?;

        let fixed_base_names = {
            let mut names = fixed_base_names(
                CERTIFICATE_VERIFICATION_KEY_NAME,
                certificate_circuit_constraint_system,
            );
            names.extend(fixed_base_names(
                IVC_VERIFICATION_KEY_NAME,
                ivc_circuit_constraint_system,
            ));
            // Remove repeated names for committed_instance and the generator
            let mut seen = HashSet::new();
            names.retain(|x| seen.insert(x.clone()));
            names
        };

        Ok(AssignedGlobal {
            genesis_message,
            genesis_verification_key,
            certificate_verification_key,
            ivc_verification_key,
            fixed_base_names,
        })
    }

    pub fn assign_state(
        &self,
        layouter: &mut impl Layouter<F>,
        state: &Value<State>,
    ) -> Result<AssignedState, Error> {
        let values = state
            .clone()
            .map(|s| {
                vec![
                    s.step_counter.as_field(),
                    s.message.as_field(),
                    s.merkle_tree_commitment.as_field(),
                    s.next_merkle_tree_commitment.as_field(),
                    s.protocol_parameters.as_field(),
                    s.next_protocol_parameters.as_field(),
                    s.current_epoch.as_field(),
                ]
            })
            .transpose_vec(7);

        let [
            step_counter,
            message,
            merkle_tree_commitment,
            next_merkle_tree_commitment,
            protocol_parameters,
            next_protocol_parameters,
            current_epoch,
        ]: [AssignedNative<_>; 7] = {
            self.native_gadget
                .assign_many(layouter, &values)?
                .try_into()
                .map_err(|v: Vec<_>| {
                    to_synthesis_error(IvcCircuitError::AssignedValueCountMismatch {
                        expected: 7,
                        actual: v.len(),
                    })
                })?
        };

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

    pub fn constrain_state_as_public_input(
        &self,
        layouter: &mut impl Layouter<F>,
        state: &AssignedState,
    ) -> Result<(), Error> {
        for value in [
            &state.step_counter,
            &state.message,
            &state.merkle_tree_commitment,
            &state.next_merkle_tree_commitment,
            &state.protocol_parameters,
            &state.next_protocol_parameters,
            &state.current_epoch,
        ] {
            self.native_gadget.constrain_as_public_input(layouter, value)?;
        }

        Ok(())
    }

    pub fn assign_witness(
        &self,
        layouter: &mut impl Layouter<F>,
        witness: &Value<Witness>,
    ) -> Result<AssignedWitness, Error> {
        let genesis_signature = {
            let s: AssignedScalarOfNativeCurve<_> = self.jubjub_chip.assign(
                layouter,
                witness.clone().map(|w| w.genesis_signature.response.0),
            )?;
            let c: AssignedNative<_> = self.native_gadget.assign(
                layouter,
                witness.clone().map(|w| w.genesis_signature.challenge.0),
            )?;
            (s, c)
        };

        let [certificate_message, certificate_merkle_tree_commitment]: [AssignedNative<F>; 2] = {
            let values = witness
                .clone()
                .map(|w| {
                    vec![
                        w.certificate_message.as_field(),
                        w.certificate_merkle_tree_commitment.as_field(),
                    ]
                })
                .transpose_vec(2);
            self.native_gadget
                .assign_many(layouter, &values)?
                .try_into()
                .map_err(|v: Vec<_>| {
                    to_synthesis_error(IvcCircuitError::AssignedValueCountMismatch {
                        expected: 2,
                        actual: v.len(),
                    })
                })?
        };

        let message_preimage = {
            let preimage = witness
                .clone()
                .map(|w| w.message_preimage.into_inner())
                .transpose_array();
            self.native_gadget.assign_many(layouter, &preimage)?
        };

        Ok(AssignedWitness {
            genesis_signature,
            certificate_merkle_tree_commitment,
            certificate_message,
            message_preimage,
        })
    }

    pub fn is_genesis(
        &self,
        layouter: &mut impl Layouter<F>,
        state: &AssignedState,
    ) -> Result<AssignedBit<F>, Error> {
        self.native_gadget.is_zero(layouter, &state.step_counter)
    }

    pub fn is_genesis_signature_valid(
        &self,
        layouter: &mut impl Layouter<F>,
        global: &AssignedGlobal,
        witness: &AssignedWitness,
    ) -> Result<AssignedBit<F>, Error> {
        let s = witness.genesis_signature.0.clone();
        let c_native = witness.genesis_signature.1.clone();
        let c: AssignedScalarOfNativeCurve<_> = self.jubjub_chip.convert(layouter, &c_native)?;

        let dst_signature: AssignedNative<_> = self
            .native_gadget
            .assign_fixed(layouter, DOMAIN_SEPARATION_TAG_STANDARD_SIGNATURE.0)?;
        let generator: AssignedNativePoint<_> = self.jubjub_chip.assign_fixed(
            layouter,
            <Jubjub as CircuitCurve>::CryptographicGroup::generator(),
        )?;

        let cap_r = self.jubjub_chip.msm(
            layouter,
            &[s, c.clone()],
            &[generator.clone(), global.genesis_verification_key.clone()],
        )?;

        let vk_x = self.jubjub_chip.x_coordinate(&global.genesis_verification_key);
        let vk_y = self.jubjub_chip.y_coordinate(&global.genesis_verification_key);
        let cap_r_x = self.jubjub_chip.x_coordinate(&cap_r);
        let cap_r_y = self.jubjub_chip.y_coordinate(&cap_r);

        let c_prime = self.poseidon_chip.hash(
            layouter,
            &[
                dst_signature.clone(),
                vk_x,
                vk_y,
                cap_r_x,
                cap_r_y,
                global.genesis_message.clone(),
            ],
        )?;

        self.native_gadget.is_equal(layouter, &c_prime, &c_native)
    }

    pub fn assert_genesis(
        &self,
        layouter: &mut impl Layouter<F>,
        is_not_genesis: &AssignedBit<F>,
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
        layouter: &mut impl Layouter<F>,
        global: &AssignedGlobal,
    ) -> Result<Vec<AssignedNative<F>>, Error> {
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
        layouter: &mut impl Layouter<F>,
        bytes: impl IntoIterator<Item = impl Into<AssignedNative<F>>>,
        bases: &[F],
    ) -> Result<AssignedNative<F>, Error> {
        let items: Vec<_> = bytes
            .into_iter()
            .zip(bases.iter())
            .map(|(v, base)| (*base, v.into()))
            .collect();

        self.native_gadget.linear_combination(layouter, &items, F::ZERO)
    }

    pub fn transition(
        &self,
        layouter: &mut impl Layouter<F>,
        is_genesis: &AssignedBit<F>,
        is_not_genesis: &AssignedBit<F>,
        global: &AssignedGlobal,
        state: &AssignedState,
        witness: &AssignedWitness,
    ) -> Result<AssignedState, Error> {
        let step_counter =
            self.native_gadget
                .add_constant(layouter, &state.step_counter, F::ONE)?;

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

        let factor = F::from(256u64);
        let bases: Vec<_> = (0..32)
            .scan(F::ONE, |s, _| {
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
        let zero = self.native_gadget.assign_fixed(layouter, F::ZERO)?;
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
            let next = self
                .native_gadget
                .add_constant(layouter, &state.current_epoch, F::ONE)?;
            let is_next_epoch = self.native_gadget.is_equal(layouter, &current_epoch, &next)?;

            (is_same_epoch, is_next_epoch)
        };

        {
            // If state.step_counter == 1, the previous certificate is a genesis certificate and
            // the current certificate is the first certificate after the genesis and
            // its epoch number must be the next epoch number.
            // Assert true: is_not_first or is_next_epoch
            let is_first =
                self.native_gadget
                    .is_equal_to_fixed(layouter, &state.step_counter, F::ONE)?;
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
            // Check the consistence on next_merkle_tree_commitment and next_protocol_parameters for certificates of the same epoch
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
        layouter: &mut impl Layouter<F>,
        global: &AssignedGlobal,
        is_not_genesis: &AssignedBit<F>,
        state: &AssignedState,
        witness: &AssignedWitness,
        certificate_proof: &Value<Vec<u8>>,
        ivc_proof: &Value<Vec<u8>>,
        acc_value: &Value<Accumulator<S>>,
    ) -> Result<AssignedAccumulator<S>, Error> {
        let id_point: AssignedForeignPoint<_, _, _> =
            self.bls12_381_chip.assign_fixed(layouter, C::identity())?;

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
        let mut next_acc = AssignedAccumulator::<S>::accumulate(
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
