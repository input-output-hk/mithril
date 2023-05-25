use mithril_stm::key_reg::KeyReg;
use mithril_stm::stm::{
    Stake, StmAggrSig, StmAggrVerificationKey, StmClerk, StmInitializer, StmParameters, StmSig,
    StmSignerAvk, StmVerificationKey,
};
use mithril_stm::AggregationError;

use blake2::{digest::consts::U32, Blake2b};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;

type H = Blake2b<U32>;

fn initialization_phase(
    nparties: u64,
    mut rng: ChaCha20Rng,
    params: StmParameters,
) -> (Vec<StmSignerAvk<H>>, Vec<(StmVerificationKey, Stake)>) {
    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::init();

    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nparties as usize);

    let mut reg_parties: Vec<(StmVerificationKey, Stake)> = Vec::with_capacity(nparties as usize);

    for stake in parties {
        let p = StmInitializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
        reg_parties.push((p.verification_key().vk, stake));
        initializers.push(p);
    }

    let closed_reg = key_reg.close();

    let signers = initializers
        .into_par_iter()
        .map(|p| p.new_signer_avk(closed_reg.clone()).unwrap())
        .collect::<Vec<StmSignerAvk<H>>>();

    (signers, reg_parties)
}

fn operation_phase(
    params: StmParameters,
    signers: Vec<StmSignerAvk<H>>,
    reg_parties: Vec<(StmVerificationKey, Stake)>,
    msg: [u8; 32],
) -> (
    Result<StmAggrSig<H>, AggregationError>,
    StmAggrVerificationKey<H>,
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
            s.verify_avk(&params, vk, stake, &avk, &msg).is_ok(),
            "Verification failed"
        );
    }

    let msig = clerk.aggregate(&sigs, &msg);

    (msig, avk)
}

#[test]
fn test_full_protocol() {
    let nparties = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);

    //////////////////////////
    // initialization phase //
    //////////////////////////

    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    let (signers, reg_parties) = initialization_phase(nparties, rng.clone(), params);
    let (msig, avk) = operation_phase(params, signers, reg_parties, msg);

    match msig {
        Ok(aggr) => {
            println!("Aggregate ok");
            assert!(aggr.verify(&msg, &avk, &params).is_ok());
        }
        Err(AggregationError::NotEnoughSignatures(n, k)) => {
            println!("Not enough signatures");
            assert!(n < params.k && k == params.k)
        }
        Err(AggregationError::UsizeConversionInvalid) => {
            println!("Invalid usize conversion");
        }
    }
}

#[test]
fn test_full_protocol_batch_verify() {
    let batch_size = 5;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

    let mut aggr_avks = Vec::new();
    let mut aggr_stms = Vec::new();
    let mut batch_msgs = Vec::new();
    let mut batch_params = Vec::new();

    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    for _ in 0..batch_size {
        let mut msg = [0u8; 32];
        rng.fill_bytes(&mut msg);
        let nparties = rng.next_u64() % 33;
        let (signers, reg_parties) = initialization_phase(nparties, rng.clone(), params);
        let operation = operation_phase(params, signers, reg_parties, msg);

        aggr_avks.push(operation.1);
        aggr_stms.push(operation.0.unwrap());
        batch_msgs.push(msg.to_vec());
        batch_params.push(params);
    }
    assert!(StmAggrSig::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params).is_ok());
}
