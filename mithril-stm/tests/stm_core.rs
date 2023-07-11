use blake2::Blake2b;
use digest::consts::U32;
use mithril_stm::stm::{
    CoreVerifier, Stake, StmInitializer, StmParameters, StmSig, StmSigner, StmVerificationKey,
};
use mithril_stm::CoreVerifierError;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
type D = Blake2b<U32>;

#[test]
fn test_core_verifier() {
    let nparties: usize = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);
    let mut public_signers: Vec<(StmVerificationKey, Stake)> = Vec::with_capacity(nparties);
    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nparties);

    //////////////////////////
    // initialization phase //
    //////////////////////////

    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    for stake in parties {
        let initializer = StmInitializer::setup(params, stake, &mut rng);
        initializers.push(initializer.clone());
        public_signers.push((initializer.verification_key().vk, initializer.stake));
    }

    let signers: Vec<StmSigner<D>> = initializers
        .into_iter()
        .filter_map(|s| s.new_core_signer(&public_signers))
        .collect();

    let core_verifier = CoreVerifier::setup(&public_signers);

    //////////////////////////
    ///// operation phase ////
    //////////////////////////

    let mut signatures: Vec<StmSig> = Vec::with_capacity(nparties);
    for s in signers {
        if let Some(sig) = s.core_sign(&msg, core_verifier.total_stake) {
            signatures.push(sig);
        }
    }
    let verify_result = core_verifier.verify(&signatures, &params, &msg);

    match verify_result {
        Ok(_) => {
            assert!(
                verify_result.is_ok(),
                "Verification failed: {verify_result:?}"
            );
        }
        Err(CoreVerifierError::NoQuorum(nr_indices, _k)) => {
            assert!((nr_indices) < params.k);
        }
        Err(CoreVerifierError::IndexNotUnique) => unreachable!(),
        _ => unreachable!(),
    }
}
