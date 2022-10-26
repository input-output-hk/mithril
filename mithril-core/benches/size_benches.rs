use blake2::digest::FixedOutput;
use blake2::{
    digest::consts::{U32, U64},
    Blake2b, Digest,
};
use mithril::key_reg::KeyReg;
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSigner};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::iter::ParallelIterator;
use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator};

fn size<H>(k: u64, m: u64, nparties: usize, hash_name: &str)
where
    H: Digest + Clone + Sync + Send + Default + FixedOutput,
{
    println!("+-------------------+");
    println!("| Hash: {} |", hash_name);
    println!("+-------------------+");
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let parties = (0..nparties)
        .into_iter()
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties);
    let params = StmParameters {
        k,
        // m equal to one, to get an upper bound were a signer can only submit a single signature
        m,
        phi_f: 0.2,
    };

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
        .collect::<Vec<StmSig<H>>>();

    let clerk = StmClerk::from_signer(&ps[0]);

    // Aggregate with random parties
    let aggr = clerk.aggregate(&sigs, &msg).unwrap();

    let sig = sigs[0].clone();

    println!(
        "k = {} | nr parties = {}; total size of single signatures {} bytes | aggregate signature {} bytes",
        k,
        nparties,
        sig.to_bytes().len() * k as usize,
        aggr.to_bytes().len() as usize,
    );
}

fn main() {
    // Upper bound on the size. We only allow one signature per party. In practice
    // a party with a lot of stake might win more than one lottery. The paths depend
    // on the number of signers, so we need to iterate over that as well
    println!("+-------------------+");
    println!("|   Size of proofs  |");
    println!("+-------------------+");
    println!("|-------------------|");
    println!("|   Trivial proofs  |");
    println!("+-------------------+");
    println!("| This gives and upper bound of the size\n| as it assumes that at most one signature\n| is provided by each participant.");
    println!("+-------------------+");

    let params: [(u64, u64, usize); 2] = [(445, 2728, 3000), (554, 3597, 3000)];
    for (k, m, nparties) in params {
        size::<Blake2b<U64>>(k, m, nparties, "Blake2b 512");
        size::<Blake2b<U32>>(k, m, nparties, "Blake2b 256");
    }
}
