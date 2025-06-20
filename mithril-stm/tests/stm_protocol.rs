mod test_extensions;

use mithril_stm::{AggregationError, StmParameters};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use test_extensions::protocol_phase::{
    initialization_phase, operation_phase, InitializationPhaseResult, OperationPhaseResult,
};

#[test]
fn test_full_protocol() {
    let nparties: usize = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);

    let params = StmParameters {
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
    }
}
