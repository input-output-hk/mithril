mod telescope_clerk;
mod telescope_proof;

// pub use telescope_clerk::*;
// pub use telescope_proof::*;

#[cfg(test)]
mod tests {
    use crate::{
        ClosedKeyRegistration, Initializer, KeyRegistration, Parameters, Signer, SingleSignature,
    };
    use alba::centralized_telescope::{Telescope, params::Params};
    use blake2::{Blake2b, digest::consts::U32};
    use rand_chacha::ChaCha20Rng;
    use rand_core::{CryptoRng, RngCore, SeedableRng};
    use rayon::prelude::*;
    const NPARTIES: usize = 1000;
    type D = Blake2b<U32>;

    fn generate_signatures(
        stm: Parameters,
        msg: &[u8],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> (SingleSignature, ClosedKeyRegistration<D>) {
        let stakes = (0..NPARTIES)
            .into_iter()
            .map(|_| 1 + (rng.next_u64() % 9999))
            .collect::<Vec<_>>();

        let mut key_reg = KeyRegistration::init();

        let mut ps: Vec<Initializer> = Vec::with_capacity(NPARTIES);
        for stake in stakes {
            let p = Initializer::new(stm, stake, rng);
            key_reg
                .register(p.stake, p.get_verification_key_proof_of_possession())
                .unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();
        println!("Key registration done");

        let ps = ps
            .into_par_iter()
            .map(|p| p.create_signer(closed_reg.clone()).unwrap())
            .collect::<Vec<Signer<D>>>();

        println!("Signers created");

        let sig = ps[0].sign(&msg).unwrap();
        // let sigs = ps
        //     .par_iter()
        //     .filter_map(|p| {
        //         return p.sign(&msg);
        //     })
        //     .collect::<Vec<SingleSignature>>();
        (sig, closed_reg)
    }

    #[test]
    fn test_telescope_stm_proof() {
        let stm = Parameters {
            m: 457263,
            k: 50001,
            phi_f: 0.2,
        };

        let telescope_params = Params {
            proof_size: 405,
            max_retries: 129,
            search_width: 32400,
            valid_proof_probability: 0.0001543,
            dfs_bound: 15316634230,
        };
        let _telescope = Telescope::setup_unsafe(50001, &telescope_params);

        // println!("{}", telescope.get_params().proof_size);

        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);

        let (sig, _closed_key_registration) = generate_signatures(stm, &msg, &mut rng);
        println!("{}", sig.indexes.len());
    }
}
