use blake2::digest::FixedOutput;
use blake2::{
    digest::consts::{U32, U64},
    Blake2b, Digest,
};
use mithril_stm::key_reg::KeyReg;
use mithril_stm::stm::{
    CoreVerifier, Stake, StmClerk, StmInitializer, StmParameters, StmSig, StmSigRegParty,
    StmSigner, StmVerificationKey,
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

    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties);
    let params = StmParameters { k, m, phi_f: 0.2 };

    let mut key_reg = KeyReg::init();
    for stake in parties {
        let p = StmInitializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
        ps.push(p);
    }

    let closed_reg = key_reg.close::<H>();

    let ps = ps
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<StmSigner<H>>>();

    let sigs = ps
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<StmSig>>();
    let clerk = StmClerk::from_signer(&ps[0]);

    // Aggregate with random parties
    let aggr = clerk.aggregate(&sigs, &msg).unwrap();

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

    let mut public_signers: Vec<(StmVerificationKey, Stake)> = Vec::with_capacity(nparties);
    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nparties);

    let parties = (0..nparties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let params = StmParameters { k, m, phi_f: 0.2 };

    for stake in parties {
        let initializer = StmInitializer::setup(params, stake, &mut rng);
        initializers.push(initializer.clone());
        public_signers.push((initializer.verification_key().vk, initializer.stake));
    }

    let core_verifier = CoreVerifier::setup(&public_signers);

    let signers: Vec<StmSigner<H>> = initializers
        .into_iter()
        .filter_map(|s| s.new_core_signer(&core_verifier.eligible_parties))
        .collect();

    let mut signatures: Vec<StmSig> = Vec::with_capacity(nparties);
    for s in signers {
        if let Some(sig) = s.core_sign(&msg, core_verifier.total_stake) {
            signatures.push(sig);
        }
    }

    let sig_reg_list = signatures
        .iter()
        .map(|sig| StmSigRegParty {
            sig: sig.clone(),
            reg_party: core_verifier.eligible_parties[sig.signer_index as usize],
        })
        .collect::<Vec<StmSigRegParty>>();

    let dedup_sigs = CoreVerifier::dedup_sigs_for_indices(
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

    println!(
        "k = {} | m = {} | nr parties = {}; {} bytes",
        k, m, nparties, size_sigs,
    );
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
