use blake2::Blake2b;
use digest::{Digest, FixedOutput};
use mithril::key_reg::KeyReg;
use mithril::stm::{StmInitializer, StmParameters};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn size<H>(hash_name: &str)
    where
        H: Digest + Clone + FixedOutput + Sync + Send,
{
    // Upper bound on the size. We only allow one signature per party. In practice
    // a party with a lot ok stake might win more than one lottery. The paths depend
    // on the number of signers, so we need to iterate over that as well
    println!("+----------------+");
    println!("| Size of proofs |");
    println!("+----------------+");
    println!("|----------------|");
    println!("| Trivial proofs |");
    println!("| Hash: {:?} |", hash_name);
    println!("+----------------+");

    static NR_K: [u64; 3] = [8, 64, 512];
    static NR_SIGNERS: [usize; 3] = [250, 2000, 32000];

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    for nparties in NR_SIGNERS {
        let parties = (0..nparties)
            .into_iter()
            .map(|_| 1 + (rng.next_u64() % 9999))
            .collect::<Vec<_>>();

        for &k in NR_K.iter() {
            let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties);
            let params = StmParameters {
                k,
                // m equal to one, to get an upper bound were a signer can only submit a single signature
                m: 1,
                phi_f: 1.0,
            };

            let mut key_reg = KeyReg::init();
            for stake in parties.clone() {
                let p = StmInitializer::setup(params, stake, &mut rng);
                key_reg.register(stake, p.verification_key()).unwrap();
                ps.push(p);
            }

            let closed_reg = key_reg.close::<H>();

            let signer = ps[0].clone().new_signer(closed_reg);
            let sig = signer.sign(&msg).unwrap();

            println!("k = {} | nr parties = {}; {} bytes", k, nparties, sig.to_bytes().len() * k as usize);
        }

    }
}

fn main() {
    size::<Blake2b>("Blake2b 512");
}
