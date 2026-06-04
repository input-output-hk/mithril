use crate::StmResult;
use anyhow::anyhow;

use super::{
    Accumulator, BinaryInstructions, C, Circuit, ComposableChip, ConstraintSystem, E, Error,
    EvaluationDomain, F, K, KZGCommitmentScheme, Layouter, NB_ARITH_COLS, NB_ARITH_FIXED_COLS,
    NB_EDWARDS_COLS, NB_POSEIDON_ADVICE_COLS, NB_POSEIDON_FIXED_COLS, NB_SHA256_ADVICE_COLS,
    NB_SHA256_FIXED_COLS, NG, PublicInputInstructions, S, SimpleFloorPlanner, Value, VerifyingKey,
    config::{IvcConfig, configure_ivc_circuit, ivc_column_pool_sizes},
    errors::IvcCircuitError,
    gadget::IvcGadget,
    nb_foreign_ecc_chip_columns,
    state::{Global, State, Witness},
    types::{CertificateProofBytes, IvcProofBytes},
};

#[derive(Clone, Debug)]
pub struct IvcCircuitData {
    // Persistent values throughout an ivc stream. This is the root of trust for an ivc stream.
    global: Value<Global>,
    // State values from the last aggregated certificate
    state: Value<State>,
    // Witness (mainly the next certificate to be aggregated) for deriving the next state
    witness: Value<Witness>,
    // Snark proof of the next certificate
    certificate_proof: Value<Vec<u8>>,
    // Latest IVC proof
    ivc_proof: Value<Vec<u8>>,
    // Latest Accumulator
    acc: Value<Accumulator<S>>,
    // Domain and ConstraintSystem associated with certificate circuit VerifyingKey
    certificate_circuit_domain_and_constraint_system: (EvaluationDomain<F>, ConstraintSystem<F>),
    // Domain and ConstraintSystem associated with IVC circuit VerifyingKey
    ivc_circuit_domain_and_constraint_system: (EvaluationDomain<F>, ConstraintSystem<F>),
}

impl IvcCircuitData {
    /// Validates that the IVC verification key degree matches the IVC circuit degree constant K.
    // Kept until the IVC prover validates recursive circuit keys.
    #[allow(dead_code)]
    pub(crate) fn validate_ivc_verification_key_degree(
        ivc_verification_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> StmResult<()> {
        let actual = ivc_verification_key.get_domain().k();
        if actual != K {
            return Err(anyhow!(IvcCircuitError::IvcVerificationKeyDegreeMismatch {
                expected: K,
                actual,
            }));
        }
        Ok(())
    }

    /// Validates that the column pool allocated by `configure_ivc_circuit` is large enough
    /// for every chip. Must be called before `Circuit::configure` is reached (e.g. in
    /// `try_new` and `unknown`) so that the `.expect` calls inside `configure_ivc_circuit`
    /// are guaranteed not to trigger.
    fn validate_column_counts() -> StmResult<()> {
        let (nb_advice_cols, nb_fixed_cols) = ivc_column_pool_sizes();

        for needed in [
            NB_ARITH_COLS,
            NB_EDWARDS_COLS,
            NB_POSEIDON_ADVICE_COLS,
            NB_SHA256_ADVICE_COLS,
            nb_foreign_ecc_chip_columns::<F, C, C, NG>(),
        ] {
            if needed > nb_advice_cols {
                return Err(anyhow!(IvcCircuitError::InsufficientAdviceColumns {
                    needed,
                    available: nb_advice_cols,
                }));
            }
        }

        for needed in [NB_ARITH_FIXED_COLS, NB_POSEIDON_FIXED_COLS, NB_SHA256_FIXED_COLS] {
            if needed > nb_fixed_cols {
                return Err(anyhow!(IvcCircuitError::InsufficientFixedColumns {
                    needed,
                    available: nb_fixed_cols,
                }));
            }
        }

        Ok(())
    }

    /// Creates a new `IvcCircuitData` with the given witness and proof data.
    ///
    /// Validates that `ivc_verification_key` has degree `K` and that the column pool allocated by
    /// `configure_ivc_circuit` is sufficient for all chips. Returns an error containing
    /// [`IvcCircuitError::IvcVerificationKeyDegreeMismatch`] or
    /// [`IvcCircuitError::InsufficientAdviceColumns`] /
    /// [`IvcCircuitError::InsufficientFixedColumns`] if either check fails.
    // Kept until the IVC prover constructs recursive circuit instances.
    #[allow(dead_code)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn try_new(
        global: Global,
        state: State,
        witness: Witness,
        certificate_proof: CertificateProofBytes,
        ivc_proof: IvcProofBytes,
        acc: Accumulator<S>,
        certificate_verification_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
        ivc_verification_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> StmResult<Self> {
        Self::validate_ivc_verification_key_degree(ivc_verification_key)?;
        Self::validate_column_counts()?;
        Ok(IvcCircuitData {
            global: Value::known(global),
            state: Value::known(state),
            witness: Value::known(witness),
            certificate_proof: Value::known(certificate_proof.into_vec()),
            ivc_proof: Value::known(ivc_proof.into_vec()),
            acc: Value::known(acc),
            certificate_circuit_domain_and_constraint_system: (
                certificate_verification_key.get_domain().clone(),
                certificate_verification_key.cs().clone(),
            ),
            ivc_circuit_domain_and_constraint_system: (
                ivc_verification_key.get_domain().clone(),
                ivc_verification_key.cs().clone(),
            ),
        })
    }

    /// Creates a default IVC circuit for generating the proving and verifying keys.
    pub fn unknown(
        certificate_verification_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> StmResult<Self> {
        Self::validate_column_counts()?;
        let mut ivc_circuit_constraint_system = ConstraintSystem::default();
        configure_ivc_circuit(&mut ivc_circuit_constraint_system);
        let ivc_circuit_domain =
            EvaluationDomain::new(ivc_circuit_constraint_system.degree() as u32, K);

        Ok(IvcCircuitData {
            global: Value::unknown(),
            state: Value::unknown(),
            witness: Value::unknown(),
            certificate_proof: Value::unknown(),
            ivc_proof: Value::unknown(),
            acc: Value::unknown(),
            certificate_circuit_domain_and_constraint_system: (
                certificate_verification_key.get_domain().clone(),
                certificate_verification_key.cs().clone(),
            ),
            ivc_circuit_domain_and_constraint_system: (
                ivc_circuit_domain,
                ivc_circuit_constraint_system,
            ),
        })
    }
}

impl Circuit<F> for IvcCircuitData {
    type Config = IvcConfig;
    type FloorPlanner = SimpleFloorPlanner;
    type Params = ();

    fn without_witnesses(&self) -> Self {
        IvcCircuitData {
            global: Value::unknown(),
            state: Value::unknown(),
            witness: Value::unknown(),
            certificate_proof: Value::unknown(),
            ivc_proof: Value::unknown(),
            acc: Value::unknown(),
            certificate_circuit_domain_and_constraint_system: self
                .certificate_circuit_domain_and_constraint_system
                .clone(),
            ivc_circuit_domain_and_constraint_system: self
                .ivc_circuit_domain_and_constraint_system
                .clone(),
        }
    }

    fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
        configure_ivc_circuit(meta)
    }

    fn synthesize(
        &self,
        config: Self::Config,
        mut layouter: impl Layouter<F>,
    ) -> Result<(), Error> {
        let ivc_gadget = IvcGadget::new(&config);

        // Assign global and constraint it as public input
        let global = ivc_gadget.assign_global_as_public_input(
            &mut layouter,
            &self.global,
            &self.certificate_circuit_domain_and_constraint_system,
            &self.ivc_circuit_domain_and_constraint_system,
        )?;
        // Assign previous state
        let state = ivc_gadget.assign_state(&mut layouter, &self.state)?;
        // Assign witness for the new certificate to be aggregated
        let witness = ivc_gadget.assign_witness(&mut layouter, &self.witness)?;

        // If state.step_counter = 0, we are aggregating the genesis certificate
        let is_genesis = ivc_gadget.is_genesis(&mut layouter, &state)?;
        let is_not_genesis = ivc_gadget.native_gadget.not(&mut layouter, &is_genesis)?;

        // Verify genesis certificate
        ivc_gadget.assert_genesis(&mut layouter, &is_not_genesis, &global, &witness)?;

        // Verify certificate chain link between the last aggregated certificate and the new certificate to obtain the next state
        let next_state = ivc_gadget.transition(
            &mut layouter,
            &is_genesis,
            &is_not_genesis,
            &global,
            &state,
            &witness,
        )?;
        // Constrain the next state as public input
        ivc_gadget.constrain_state_as_public_input(&mut layouter, &next_state)?;

        // Verify (prepare) certificate_proof and previous ivc_proof and update accumulator
        let next_acc = ivc_gadget.verify_prepare(
            &mut layouter,
            &global,
            &is_not_genesis,
            &state,
            &witness,
            &self.certificate_proof,
            &self.ivc_proof,
            &self.acc,
        )?;
        // Constrain the next accumulator as public input
        ivc_gadget
            .verifier_gadget
            .constrain_as_public_input(&mut layouter, &next_acc)?;

        ivc_gadget.core_decomp_chip.load(&mut layouter)?;
        ivc_gadget.sha2_256_chip.load(&mut layouter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ivc_circuit_constraint_count() {
        let mut cs = ConstraintSystem::<F>::default();
        configure_ivc_circuit(&mut cs);

        let poly_constraints: usize = cs.gates().iter().map(|g| g.polynomials().len()).sum();
        assert_eq!(
            K, 19,
            "circuit size k must not change without a deliberate decision"
        );
        assert_eq!(
            poly_constraints, 53,
            "polynomial constraint count must not silently grow"
        );
        assert_eq!(
            cs.lookups().len(),
            7,
            "lookup argument count must not silently grow"
        );
    }
}
