use blake2::{digest::consts::U32, Blake2b};
use mithril_stm::{
    AggregationError, KeyReg, Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer,
    StmParameters, StmSig, StmSigner, StmVerificationKey,
};
use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;
use rayon::prelude::*;

type H = Blake2b<U32>;

pub fn initialization_phase(
    nparties: usize,
    mut rng: ChaCha20Rng,
    params: StmParameters,
) -> (
    Vec<StmSigner<H>>,
    Vec<(StmVerificationKey, Stake)>,
    Vec<StmInitializer>,
) {
    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::init();

    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nparties);

    let mut reg_parties: Vec<(StmVerificationKey, Stake)> = Vec::with_capacity(nparties);

    for stake in parties {
        let p = StmInitializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
        reg_parties.push((p.verification_key().vk, stake));
        initializers.push(p);
    }

    let closed_reg = key_reg.close();

    let signers = initializers
        .clone()
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<StmSigner<H>>>();

    (signers, reg_parties, initializers)
}

pub fn operation_phase(
    params: StmParameters,
    signers: Vec<StmSigner<H>>,
    reg_parties: Vec<(StmVerificationKey, Stake)>,
    msg: [u8; 32],
) -> (
    Result<StmAggrSig<H>, AggregationError>,
    StmAggrVerificationKey<H>,
    Vec<StmSig>,
) {
    let sigs = signers
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<StmSig>>();

    let clerk = StmClerk::from_signer(&signers[0]);
    let avk = clerk.compute_avk();

    // Check all parties can verify every sig
    for (s, (vk, stake)) in sigs.iter().zip(reg_parties.iter()) {
        assert!(
            s.verify(&params, vk, stake, &avk, &msg).is_ok(),
            "Verification failed"
        );
    }

    let msig = clerk.aggregate(&sigs, &msg);

    (msig, avk, sigs)
}
