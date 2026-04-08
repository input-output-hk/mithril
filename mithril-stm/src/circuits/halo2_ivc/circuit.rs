use super::{
    Accumulator, BinaryInstructions, Circuit, ComposableChip, ConstraintSystem, E,
    EvaluationDomain, Error, F, K, KZGCommitmentScheme, Layouter, PublicInputInstructions, S,
    SimpleFloorPlanner, Value, VerifyingKey,
    config::{IvcConfig, configure_ivc_circuit},
    gadget::IvcGadget,
    state::{Global, State, Witness},
};

#[derive(Clone, Debug)]
pub struct IvcCircuit {
    // Persistent values throughout an ivc stream. This is the root of trust for an ivc stream.
    pub global: Value<Global>,
    // State values from the last aggregated certificate
    pub state: Value<State>,
    // Witness (mainly the next certificate to be aggregated) for deriving the next state
    pub witness: Value<Witness>,
    // Snark proof of the next certificate
    pub cert_proof: Value<Vec<u8>>,
    // Latest IVC proof
    pub self_proof: Value<Vec<u8>>,
    // Latest Accumulator
    pub acc: Value<Accumulator<S>>,
    // Domain and ConstraintSystem associated with certificate circuit VerifyingKey
    pub cert_domain_cs: (EvaluationDomain<F>, ConstraintSystem<F>),
    // Domain and ConstraintSystem associated with ivc circuit VerifyingKey
    pub self_domain_cs: (EvaluationDomain<F>, ConstraintSystem<F>),
}

impl IvcCircuit {
    pub fn new(
        global: Global,
        state: State,
        witness: Witness,
        cert_proof: Vec<u8>,
        self_proof: Vec<u8>,
        acc: Accumulator<S>,
        cert_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
        self_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    ) -> Self {
        IvcCircuit {
            global: Value::known(global),
            state: Value::known(state),
            witness: Value::known(witness),
            cert_proof: Value::known(cert_proof),
            self_proof: Value::known(self_proof),
            acc: Value::known(acc),
            cert_domain_cs: (cert_vk.get_domain().clone(), cert_vk.cs().clone()),
            self_domain_cs: (self_vk.get_domain().clone(), self_vk.cs().clone()),
        }
    }

    // Create a default ivc circuit for generating ivc proving key and verifying key
    pub fn unknown(cert_vk: &VerifyingKey<F, KZGCommitmentScheme<E>>) -> Self {
        let mut self_cs = ConstraintSystem::default();
        configure_ivc_circuit(&mut self_cs);
        let self_domain = EvaluationDomain::new(self_cs.degree() as u32, K);

        IvcCircuit {
            global: Value::unknown(),
            state: Value::unknown(),
            witness: Value::unknown(),
            cert_proof: Value::unknown(),
            self_proof: Value::unknown(),
            acc: Value::unknown(),
            cert_domain_cs: (cert_vk.get_domain().clone(), cert_vk.cs().clone()),
            self_domain_cs: (self_domain, self_cs),
        }
    }
}

impl Circuit<F> for IvcCircuit {
    type Config = IvcConfig;
    type FloorPlanner = SimpleFloorPlanner;
    type Params = ();

    fn without_witnesses(&self) -> Self {
        unreachable!()
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
            &self.cert_domain_cs,
            &self.self_domain_cs,
        )?;
        // Assign previous state
        let state = ivc_gadget.assign_state(&mut layouter, &self.state)?;
        // Assign witness for the new certificate to be aggregated
        let witness = ivc_gadget.assign_witness(&mut layouter, &self.witness)?;

        // If state.counter = 0, we are aggregating the genesis certificate
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

        // Verify (prepare) cert_proof and previous ivc_proof and update accumulator
        let next_acc = ivc_gadget.verify_prepare(
            &mut layouter,
            &global,
            &is_not_genesis,
            &state,
            &witness,
            &self.cert_proof,
            &self.self_proof,
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
