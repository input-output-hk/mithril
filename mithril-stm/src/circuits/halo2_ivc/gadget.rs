use super::{
    Accumulator, ArithInstructions, AssertionInstructions, AssignedAccumulator, AssignedBit,
    AssignedForeignPoint, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
    AssignedVk, AssignmentInstructions, BinaryInstructions, C, CERT_VK_NAME, CircuitCurve,
    ComposableChip, ConstraintSystem, ControlFlowInstructions, ConversionInstructions,
    DST_SCHNORR_SIGNATURE, EccChip, EccInstructions, EqualityInstructions, Error, EvaluationDomain,
    F, ForeignEccChip, HashInstructions, IVC_ONE_NAME, Jubjub, K, Layouter, NG, NativeChip,
    NativeGadget, P2RDecompositionChip, PoseidonChip, PublicInputInstructions, S, Value,
    VerifierGadget, ZeroInstructions,
    config::IvcConfig,
    state::{
        AssignedGlobal, AssignedState, AssignedWitness, Global, State, Witness, fixed_base_names,
    },
};
use ff::Field;
use group::Group;
use std::collections::HashSet;

use midnight_circuits::hash::sha256::Sha256Chip;
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
        cert_domain_cs: &(EvaluationDomain<F>, ConstraintSystem<F>),
        self_domain_cs: &(EvaluationDomain<F>, ConstraintSystem<F>),
    ) -> Result<AssignedGlobal, Error> {
        let genesis_msg: AssignedNative<_> = self
            .native_gadget
            .assign_as_public_input(layouter, global.clone().map(|gl| gl.genesis_msg))?;
        let genesis_vk: AssignedNativePoint<_> = self
            .jubjub_chip
            .assign_as_public_input(layouter, global.clone().map(|gl| gl.genesis_vk.0))?;

        let (cert_domain, cert_cs) = &cert_domain_cs;
        let cert_vk: AssignedVk<S> = self.verifier_gadget.assign_vk_as_public_input(
            layouter,
            CERT_VK_NAME,
            cert_domain,
            cert_cs,
            global.clone().map(|gl| gl.cert_vk_repr),
        )?;

        // Assign for self-proof verification
        let (self_domain, self_cs) = &self_domain_cs;
        let self_vk: AssignedVk<S> = self.verifier_gadget.assign_vk_as_public_input(
            layouter,
            IVC_ONE_NAME,
            self_domain,
            self_cs,
            global.clone().map(|gl| gl.self_vk_repr),
        )?;

        let fixed_base_names = {
            let mut names = fixed_base_names(CERT_VK_NAME, cert_cs);
            names.extend(fixed_base_names(IVC_ONE_NAME, self_cs));
            // Remove repeated names for committed_instance and the generator
            let mut seen = HashSet::new();
            names.retain(|x| seen.insert(x.clone()));
            names
        };

        Ok(AssignedGlobal {
            genesis_msg,
            genesis_vk,
            cert_vk,
            self_vk,
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
                    s.counter,
                    s.msg,
                    s.merkle_root,
                    s.next_merkle_root,
                    s.protocol_params,
                    s.next_protocol_params,
                    s.current_epoch,
                ]
            })
            .transpose_vec(7);

        let [
            counter,
            msg,
            merkle_root,
            next_merkle_root,
            protocol_params,
            next_protocol_params,
            current_epoch,
        ]: [AssignedNative<_>; 7] = {
            self.native_gadget
                .assign_many(layouter, &values)?
                .try_into()
                // this won't fail
                .unwrap()
        };

        Ok(AssignedState {
            counter,
            msg,
            merkle_root,
            next_merkle_root,
            protocol_params,
            next_protocol_params,
            current_epoch,
        })
    }

    pub fn constrain_state_as_public_input(
        &self,
        layouter: &mut impl Layouter<F>,
        state: &AssignedState,
    ) -> Result<(), Error> {
        for value in [
            &state.counter,
            &state.msg,
            &state.merkle_root,
            &state.next_merkle_root,
            &state.protocol_params,
            &state.next_protocol_params,
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
        let genesis_sig = {
            let s: AssignedScalarOfNativeCurve<_> = self
                .jubjub_chip
                .assign(layouter, witness.clone().map(|w| w.genesis_sig.s))?;
            let c: AssignedNative<_> = self
                .native_gadget
                .assign(layouter, witness.clone().map(|w| w.genesis_sig.c))?;
            (s, c)
        };

        let [cert_msg, cert_merkle_root]: [AssignedNative<F>; 2] = {
            let values = witness
                .clone()
                .map(|w| vec![w.cert_msg, w.cert_merkle_root])
                .transpose_vec(2);
            self.native_gadget
                .assign_many(layouter, &values)?
                .try_into()
                // this won't fail
                .unwrap()
        };

        let msg_preimage = {
            let preimage = witness.clone().map(|w| w.msg_preimage).transpose_array();
            self.native_gadget.assign_many(layouter, &preimage)?
        };

        Ok(AssignedWitness {
            genesis_sig,
            cert_merkle_root,
            cert_msg,
            msg_preimage,
        })
    }

    pub fn is_genesis(
        &self,
        layouter: &mut impl Layouter<F>,
        state: &AssignedState,
    ) -> Result<AssignedBit<F>, Error> {
        self.native_gadget.is_zero(layouter, &state.counter)
    }

    pub fn is_genesis_sig_valid(
        &self,
        layouter: &mut impl Layouter<F>,
        global: &AssignedGlobal,
        witness: &AssignedWitness,
    ) -> Result<AssignedBit<F>, Error> {
        let s = witness.genesis_sig.0.clone();
        let c_native = witness.genesis_sig.1.clone();
        let c: AssignedScalarOfNativeCurve<_> = self.jubjub_chip.convert(layouter, &c_native)?;

        let dst_signature: AssignedNative<_> =
            self.native_gadget.assign_fixed(layouter, DST_SCHNORR_SIGNATURE)?;
        let generator: AssignedNativePoint<_> = self.jubjub_chip.assign_fixed(
            layouter,
            <Jubjub as CircuitCurve>::CryptographicGroup::generator(),
        )?;

        let cap_r = self.jubjub_chip.msm(
            layouter,
            &[s, c.clone()],
            &[generator.clone(), global.genesis_vk.clone()],
        )?;

        let vk_x = self.jubjub_chip.x_coordinate(&global.genesis_vk);
        let vk_y = self.jubjub_chip.y_coordinate(&global.genesis_vk);
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
                global.genesis_msg.clone(),
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
        let is_genesis_sig_valid = self.is_genesis_sig_valid(layouter, global, witness)?;

        // Skip the genesis signature verification if it is not genesis
        let check_genesis = self
            .native_gadget
            .or(layouter, &[is_genesis_sig_valid, is_not_genesis.clone()])?;
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
                global.genesis_msg.clone(),
                self.jubjub_chip.x_coordinate(&global.genesis_vk),
                self.jubjub_chip.y_coordinate(&global.genesis_vk),
            ],
            self.verifier_gadget.as_public_input(layouter, &global.cert_vk)?,
            self.verifier_gadget.as_public_input(layouter, &global.self_vk)?,
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
        let counter = self.native_gadget.add_constant(layouter, &state.counter, F::ONE)?;

        let (cert_msg, cert_merkle_root) =
            (witness.cert_msg.clone(), witness.cert_merkle_root.clone());

        // Open msg hash to check the link between certificates
        // If it is genesis, select genesis msg as msg; otherwise, select cert msg as msg.
        let msg =
            self.native_gadget
                .select(layouter, is_genesis, &global.genesis_msg, &cert_msg)?;

        let hash = self.sha2_256_chip.hash(layouter, &witness.msg_preimage)?;

        let factor = F::from(256u64);
        let bases: Vec<_> = (0..32)
            .scan(F::ONE, |s, _| {
                let out = *s;
                *s *= factor;
                Some(out)
            })
            .collect();

        {
            // Compare msg and hash
            let hash_native = self.combine_bytes(layouter, &hash, &bases)?;
            self.native_gadget.assert_equal(layouter, &msg, &hash_native)?;
        }

        // If it is genesis, merkle_root = 0; otherwise, merkle_root = cert_merkle_root.
        let zero = self.native_gadget.assign_fixed(layouter, F::ZERO)?;
        let merkle_root =
            self.native_gadget
                .select(layouter, is_genesis, &zero, &cert_merkle_root)?;

        // Read the value of next merkle root, next protocol parameters and current epoch from protocol message preimage
        // digest(6) | bytes(32) | next_aggregate_verification_key(31) | bytes(44) | next_protocol_parameters(24) | bytes(32) | current_epoch(13) | bytes(8)
        // todo: check field keywords(?)
        let next_merkle_root_bytes = witness.msg_preimage[69..101].to_vec();
        let next_protocol_params_bytes = witness.msg_preimage[137..169].to_vec();
        let current_epoch_bytes = witness.msg_preimage[182..190].to_vec();

        // Get the field elements by linearly combining the bytes
        let (next_merkle_root, next_protocol_params, current_epoch) = {
            let next_merkle_root = self.combine_bytes(layouter, next_merkle_root_bytes, &bases)?;
            let next_protocol_params =
                self.combine_bytes(layouter, next_protocol_params_bytes, &bases)?;
            let current_epoch = self.combine_bytes(layouter, current_epoch_bytes, &bases)?;
            (next_merkle_root, next_protocol_params, current_epoch)
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
            // If state.counter == 1, the previous certificate is a genesis certificate and
            // the current certificate is the first certificate after the genesis and
            // its epoch number must be the next epoch number.
            // Assert true: is_not_first or is_next_epoch
            let is_first =
                self.native_gadget
                    .is_equal_to_fixed(layouter, &state.counter, F::ONE)?;
            let is_not_first = self.native_gadget.not(layouter, &is_first)?;

            let is_valid = self
                .native_gadget
                .or(layouter, &[is_not_first, is_next_epoch.clone()])?;
            self.native_gadget.assert_equal_to_fixed(layouter, &is_valid, true)?;
        }

        {
            // Check the link on the current merkle root; if it is genesis, skip the checking
            // Assert true: is_genesis or (is_same_epoch && merkle_root == state.merkle_root) or (is_next_epoch && merkle_root == state.next_merkle_root)
            let mut is_equal_current =
                self.native_gadget
                    .is_equal(layouter, &merkle_root, &state.merkle_root)?;
            is_equal_current = self
                .native_gadget
                .and(layouter, &[is_equal_current, is_same_epoch.clone()])?;

            let mut is_equal_next =
                self.native_gadget
                    .is_equal(layouter, &merkle_root, &state.next_merkle_root)?;
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

        let protocol_params = {
            // If genesis: protocol_params = 0
            // Else:
            //     if same_epoch: protocol_params = state.protocol_params;
            //     else: protocol_params = state.next_protocol_params
            let mut protocol_params = self.native_gadget.select(
                layouter,
                is_genesis,
                &zero,
                &state.next_protocol_params,
            )?;
            let is_same_epoch_not_genesis = self
                .native_gadget
                .and(layouter, &[is_same_epoch.clone(), is_not_genesis.clone()])?;
            protocol_params = self.native_gadget.select(
                layouter,
                &is_same_epoch_not_genesis,
                &state.protocol_params,
                &protocol_params,
            )?;
            protocol_params
        };

        {
            // Check the consistence on next_merkle_root and next_protocol_params for certificates of the same epoch
            // Assert true: is_genesis or (is_same_epoch && next_merkle_root == state.next_merkle_root && next_protocol_params == state.next_protocol_params) or is_next_epoch
            let is_equal_mt = self.native_gadget.is_equal(
                layouter,
                &next_merkle_root,
                &state.next_merkle_root,
            )?;
            let is_equal_pp = self.native_gadget.is_equal(
                layouter,
                &next_protocol_params,
                &state.next_protocol_params,
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
            counter,
            msg,
            merkle_root,
            next_merkle_root,
            protocol_params,
            next_protocol_params,
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
        cert_proof: &Value<Vec<u8>>,
        self_proof: &Value<Vec<u8>>,
        acc_value: &Value<Accumulator<S>>,
    ) -> Result<AssignedAccumulator<S>, Error> {
        let id_point: AssignedForeignPoint<_, _, _> =
            self.bls12_381_chip.assign_fixed(layouter, C::identity())?;

        let mut cert_proof_acc = self.verifier_gadget.prepare(
            layouter,
            &global.cert_vk,
            &[("com_instance", id_point.clone())],
            &[&[witness.cert_merkle_root.clone(), witness.cert_msg.clone()]],
            cert_proof.clone(),
        )?;

        // If it is genesis, we allow the prover to change the (probably
        // invalid) accumulator by a default accumulator that satisfies the invariant.
        AssignedAccumulator::scale_by_bit(
            layouter,
            &self.native_gadget,
            is_not_genesis,
            &mut cert_proof_acc,
        )?;
        cert_proof_acc.collapse(layouter, &self.bls12_381_chip, &self.native_gadget)?;

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

        // Verify a witnessed proof that ensures the validity of previous state.
        // The proof is valid iff `proof_acc` satisfies the invariant.
        let mut self_proof_acc = self.verifier_gadget.prepare(
            layouter,
            &global.self_vk,
            &[("com_instance", id_point)],
            &[&assigned_pi],
            self_proof.clone(),
        )?;

        // If the previous state is genesis, we allow the prover to change the (probably
        // invalid) accumulator by a default accumulator that satisfies the invariant.
        AssignedAccumulator::scale_by_bit(
            layouter,
            &self.native_gadget,
            is_not_genesis,
            &mut self_proof_acc,
        )?;
        self_proof_acc.collapse(layouter, &self.bls12_381_chip, &self.native_gadget)?;

        // Accumulate the cert_proof_acc
        let mut next_acc = AssignedAccumulator::<S>::accumulate(
            layouter,
            &self.verifier_gadget,
            &self.native_gadget,
            &self.poseidon_chip,
            &[acc, cert_proof_acc, self_proof_acc],
        )?;
        next_acc.collapse(layouter, &self.bls12_381_chip, &self.native_gadget)?;

        Ok(next_acc)
    }
}
