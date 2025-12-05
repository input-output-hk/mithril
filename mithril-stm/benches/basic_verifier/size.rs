use blake2::digest::FixedOutput;
use blake2::{Blake2b, Digest, digest::consts::U64};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use mithril_stm::{
    BasicVerifier, Initializer, Parameters, Signer, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, VerificationKey,
};

fn core_size<H>(k: u64, m: u64, nparties: usize)
where
    H: Digest + Clone + Sync + Send + Default + FixedOutput,
{
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let mut public_signers: Vec<(VerificationKey, Stake)> = Vec::with_capacity(nparties);
    let mut initializers: Vec<Initializer> = Vec::with_capacity(nparties);

    let parties = (0..nparties).map(|_| 1 + (rng.next_u64() % 9999)).collect::<Vec<_>>();

    let params = Parameters { k, m, phi_f: 0.2 };

    for stake in parties {
        let initializer = Initializer::new(params, stake, &mut rng);
        initializers.push(initializer.clone());
        public_signers.push((
            initializer.get_verification_key_proof_of_possession().vk,
            initializer.stake,
        ));
    }

    let core_verifier = BasicVerifier::new(&public_signers);

    let signers: Vec<Signer<H>> = initializers
        .into_iter()
        .filter_map(|s| s.create_basic_signer(&core_verifier.eligible_parties))
        .collect();

    let mut signatures: Vec<SingleSignature> = Vec::with_capacity(nparties);
    for s in signers {
        if let Some(sig) = s.basic_sign(&msg, core_verifier.total_stake) {
            signatures.push(sig);
        }
    }

    let sig_reg_list = signatures
        .iter()
        .map(|sig| SingleSignatureWithRegisteredParty {
            sig: sig.clone(),
            reg_party: core_verifier.eligible_parties[sig.signer_index as usize],
        })
        .collect::<Vec<SingleSignatureWithRegisteredParty>>();

    let dedup_sigs = BasicVerifier::select_valid_signatures_for_k_indices(
        &core_verifier.total_stake,
        &params,
        &msg,
        &sig_reg_list,
    )
    .unwrap();

    let mut size_sigs: usize = 0;
    for sig in dedup_sigs {
        size_sigs += sig.to_bytes().len();
    }

    println!("k = {k} | m = {m} | nr parties = {nparties}; {size_sigs} bytes",);
}

fn main() {
    println!("\n+-------------------------+");
    println!("| Size of core signatures |");
    println!("+-------------------------+");
    println!("+-------------------------+");
    let params: [(u64, u64, usize); 2] = [(445, 2728, 3000), (554, 3597, 3000)];
    for (k, m, nparties) in params {
        core_size::<Blake2b<U64>>(k, m, nparties);
    }
    println!("+-------------------------+");
}
