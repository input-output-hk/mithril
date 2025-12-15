use blake2::{Blake2b, digest::consts::U32};
use mithril_stm::{
    AggregateSignature, AggregateSignatureType, AggregateVerificationKey, Clerk, Initializer,
    KeyRegistration, Parameters, Signer, SingleSignature, Stake, StmResult, VerificationKey,
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
    pub msig: StmResult<AggregateSignature<H>>,
    pub avk: AggregateVerificationKey<H>,
    pub sigs: Vec<SingleSignature>,
}

pub fn initialization_phase(
    nparties: usize,
    mut rng: ChaCha20Rng,
    params: Parameters,
) -> InitializationPhaseResult {
    let parties = (0..nparties).map(|_| 1 + (rng.next_u64() % 9999)).collect::<Vec<_>>();

    let mut key_reg = KeyRegistration::init();

    let mut initializers: Vec<Initializer> = Vec::with_capacity(nparties);

    let mut reg_parties: Vec<(VerificationKey, Stake)> = Vec::with_capacity(nparties);

    for stake in parties {
        let p = Initializer::new(params, stake, &mut rng);
        key_reg
            .register(stake, p.get_verification_key_proof_of_possession())
            .unwrap();
        reg_parties.push((p.get_verification_key_proof_of_possession().vk, stake));
        initializers.push(p);
    }

    let closed_reg = key_reg.close();

    let signers = initializers
        .clone()
        .into_par_iter()
        .map(|p| p.create_signer(closed_reg.clone()).unwrap())
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

    let clerk = Clerk::new_clerk_from_signer(&signers[0]);
    let avk = clerk.compute_aggregate_verification_key();
    let aggr_sig_type = AggregateSignatureType::Concatenation;

    // Check all parties can verify every sig
    for (s, (vk, stake)) in sigs.iter().zip(reg_parties.iter()) {
        assert!(
            s.verify(&params, vk, stake, &avk, &msg).is_ok(),
            "Verification failed"
        );
    }

    let msig = clerk.aggregate_signatures_with_type(&sigs, &msg, aggr_sig_type);

    OperationPhaseResult { msig, avk, sigs }
}
