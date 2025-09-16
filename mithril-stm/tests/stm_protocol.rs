mod test_extensions;

use mithril_stm::{AggregateSignature, AggregationError, Parameters};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use test_extensions::protocol_phase::{
    InitializationPhaseResult, OperationPhaseResult, initialization_phase, operation_phase,
};

#[test]
fn test_full_protocol() {
    let nparties: usize = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);

    let params = Parameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    let InitializationPhaseResult {
        signers,
        reg_parties,
        initializers: _,
    } = initialization_phase(nparties, rng.clone(), params);
    let OperationPhaseResult { msig, avk, sigs: _ } =
        operation_phase(params, signers, reg_parties, msg);

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
        Err(AggregationError::UnsupportedProofSystem(_)) => {
            println!("Unsupported proof system");
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

    let params = Parameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    for _ in 0..batch_size {
        let mut msg = [0u8; 32];
        rng.fill_bytes(&mut msg);
        let nparties = rng.next_u64() % 33;
        let InitializationPhaseResult {
            signers,
            reg_parties,
            initializers: _,
        } = initialization_phase(nparties as usize, rng.clone(), params);
        let OperationPhaseResult { msig, avk, sigs: _ } =
            operation_phase(params, signers, reg_parties, msg);

        aggr_avks.push(avk);
        aggr_stms.push(msig.unwrap());
        batch_msgs.push(msg.to_vec());
        batch_params.push(params);
    }
    assert!(
        AggregateSignature::batch_verify(&aggr_stms, &batch_msgs, &aggr_avks, &batch_params)
            .is_ok()
    );
}
