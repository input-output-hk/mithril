use blake2::digest::FixedOutput;
use blake2::{
    Blake2b, Digest,
    digest::consts::{U32, U64},
};
use mithril_stm::{
    AggregateSignatureType, BasicVerifier, Clerk, Initializer, KeyRegistration, Parameters, Signer,
    SingleSignature, SingleSignatureWithRegisteredParty, Stake, VerificationKey,
};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::iter::ParallelIterator;
use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator};

fn size<H>(k: u64, m: u64, nparties: usize, hash_name: &str)
where
    H: Digest + Clone + Sync + Send + Default + FixedOutput,
{
    println!("+-------------------+");
    println!("| Hash: {hash_name} |");
    println!("+-------------------+");
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let parties = (0..nparties).map(|_| 1 + (rng.next_u64() % 9999)).collect::<Vec<_>>();

    let mut ps: Vec<Initializer> = Vec::with_capacity(nparties);
    let params = Parameters { k, m, phi_f: 0.2 };

    let mut key_reg = KeyRegistration::init();
    for stake in parties {
        let p = Initializer::new(params, stake, &mut rng);
        key_reg
            .register(stake, p.get_verification_key_proof_of_possession())
            .unwrap();
        ps.push(p);
    }

    let closed_reg = key_reg.close::<H>();

    let ps = ps
        .into_par_iter()
        .map(|p| p.create_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<Signer<H>>>();

    let sigs = ps
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<SingleSignature>>();
    let clerk = Clerk::new_clerk_from_signer(&ps[0]);

    // Aggregate with random parties
    let aggr_sig_type = AggregateSignatureType::Concatenation;
    let aggr = clerk
        .aggregate_signatures_with_type(&sigs, &msg, aggr_sig_type)
        .unwrap();

    println!(
        "k = {} | m = {} | nr parties = {}; {} bytes",
        k,
        m,
        nparties,
        aggr.to_bytes().len(),
    );
}

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
    println!("+-------------------+");
    println!("|   Size of proofs  |");
    println!("+-------------------+");
    println!("| Results obtained by using the parameters suggested in paper.");
    println!("+-------------------+");

    let params: [(u64, u64, usize); 2] = [(445, 2728, 3000), (554, 3597, 3000)];
    for (k, m, nparties) in params {
        size::<Blake2b<U64>>(k, m, nparties, "Blake2b 512");
        size::<Blake2b<U32>>(k, m, nparties, "Blake2b 256");
    }

    println!("\n+-------------------------+");
    println!("| Size of core signatures |");
    println!("+-------------------------+");
    println!("+-------------------------+");

    for (k, m, nparties) in params {
        core_size::<Blake2b<U64>>(k, m, nparties);
    }
    println!("+-------------------------+");
}
