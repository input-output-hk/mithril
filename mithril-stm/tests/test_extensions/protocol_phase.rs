use blake2::{digest::consts::U32, Blake2b};
use mithril_stm::{
    AggregateSignature, AggregateVerificationKey, AggregationError, Clerk, Initializer,
    KeyRegistration, Parameters, Signer, SingleSignature, Stake, VerificationKey,
};
use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;
use rayon::prelude::*;

type H = Blake2b<U32>;

/// The result of the initialization phase of the STM protocol.
pub struct InitializationPhaseResult {
    pub signers: Vec<Signer<H>>,
    pub reg_parties: Vec<(VerificationKey, Stake)>,
    pub initializers: Vec<Initializer>,
}

/// The result of the operation phase of the STM protocol.
pub struct OperationPhaseResult {
    pub msig: Result<AggregateSignature<H>, AggregationError>,
    pub avk: AggregateVerificationKey<H>,
    pub sigs: Vec<SingleSignature>,
}

pub fn initialization_phase(
    nparties: usize,
    mut rng: ChaCha20Rng,
    params: Parameters,
) -> InitializationPhaseResult {
    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut key_reg = KeyRegistration::init();

    let mut initializers: Vec<Initializer> = Vec::with_capacity(nparties);

    let mut reg_parties: Vec<(VerificationKey, Stake)> = Vec::with_capacity(nparties);

    for stake in parties {
        let p = Initializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
        reg_parties.push((p.verification_key().vk, stake));
        initializers.push(p);
    }

    let closed_reg = key_reg.close();

    let signers = initializers
        .clone()
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<Signer<H>>>();

    InitializationPhaseResult {
        signers,
        reg_parties,
        initializers,
    }
}

pub fn operation_phase(
    params: Parameters,
    signers: Vec<Signer<H>>,
    reg_parties: Vec<(VerificationKey, Stake)>,
    msg: [u8; 32],
) -> OperationPhaseResult {
    let sigs = signers
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<SingleSignature>>();

    let clerk = Clerk::from_signer(&signers[0]);
    let avk = clerk.compute_avk();

    // Check all parties can verify every sig
    for (s, (vk, stake)) in sigs.iter().zip(reg_parties.iter()) {
        assert!(
            s.verify(&params, vk, stake, &avk, &msg).is_ok(),
            "Verification failed"
        );
    }

    let msig = clerk.aggregate(&sigs, &msg);

    OperationPhaseResult { msig, avk, sigs }
}
