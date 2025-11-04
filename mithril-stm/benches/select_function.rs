use std::{collections::BTreeMap, process::id, time::Instant};

use blake2::{Blake2b, digest::consts::U32};
use mithril_stm::{
    AggregateSignature, AggregateVerificationKey, AggregationError, BasicVerifier, Clerk,
    Initializer, KeyRegistration, Parameters, Signer, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, VerificationKey,
};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::{prelude::*, vec};

type H = Blake2b<U32>;

pub fn select_sig_test() {
    let nparties = 3000; // Use a small number of parties for this example
    type D = Blake2b<U32>; // Setting the hash function for convenience

    // let mut rng = ChaCha20Rng::from_seed([0u8; 32]); // create and initialize rng
    let mut rng = ChaCha20Rng::from_entropy();
    let mut msg = [0u8; 16]; // setting an arbitrary message
    rng.fill_bytes(&mut msg);

    let params = Parameters {
        m: 2728,    // Security parameter XXX: not for production
        k: 445,     // Quorum parameter XXX: not for production
        phi_f: 0.2, // Lottery parameter XXX: not for production
    };

    // Generate some arbitrary stake for each party
    // Stake is an integer.
    // Total stake of all parties is total stake in the system.
    let stakes = (0..nparties)
        .into_iter()
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    // Create a new key registry from the parties and their stake
    let mut key_reg = KeyRegistration::init();

    // For each party, crate a Initializer.
    // This struct can create keys for the party.
    let mut ps: Vec<Initializer> = Vec::with_capacity(nparties);
    for stake in stakes {
        // Create keys for this party
        let p = Initializer::new(params, stake, &mut rng);
        // Register keys with the KeyRegistration service
        key_reg
            .register(p.stake, p.get_verification_key_proof_of_possession())
            .unwrap();
        ps.push(p);
    }

    let closed_reg = key_reg.close();

    // println!("{:?}", closed_reg.merkle_tree);

    let ps = ps
        .into_par_iter()
        .map(|p| p.create_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<Signer<D>>>();

    // println!("Number of Signers: {:?}", ps.len());

    let sigs = ps
        .par_iter()
        .filter_map(|p| {
            return p.sign(&msg);
        })
        .collect::<Vec<SingleSignature>>();

    // Clerk can aggregate and verify signatures.
    let clerk = Clerk::new_clerk_from_closed_key_registration(&params, &closed_reg);

    // Aggregate and verify the signatures
    let sig_reg_list = sigs
        .iter()
        .map(|sig| SingleSignatureWithRegisteredParty {
            sig: sig.clone(),
            reg_party: closed_reg.reg_parties[sig.signer_index as usize],
        })
        .collect::<Vec<SingleSignatureWithRegisteredParty>>();

    let avk = AggregateVerificationKey::from(&closed_reg);
    let msgp = avk.get_mt_commitment().concat_with_msg(&msg);

    // println!("NEW function!");
    // let (idx_by_mtidx, btm) =
    //     BasicVerifier::get_k_indices(&closed_reg.total_stake, &params, &msgp, &sig_reg_list);
    // // println!("list idx: {:?}", idx_by_mtidx);
    // // println!("nb of idx: {:?}", idx_by_mtidx.len());
    // let mut vec_single_sig =
    //     BasicVerifier::valid_signatures_from_k_indices(&params, idx_by_mtidx, btm).unwrap();
    // vec_single_sig.sort_unstable();
    let nb_loop = 20;
    let now = Instant::now();
    let mut time_rework = 0;
    let mut time_rework_opti = 0;
    for _ in 0..nb_loop {
        let now = Instant::now();
        let _ = BasicVerifier::reworked_select_valid_signatures_for_k_indices(
            &closed_reg.total_stake,
            &params,
            &msgp,
            &sig_reg_list,
        )
        .unwrap();
        time_rework += now.elapsed().as_millis();
        let now = Instant::now();
        let _ = BasicVerifier::reworked_select_valid_signatures_for_k_indices_opti(
            &closed_reg.total_stake,
            &params,
            &msgp,
            &sig_reg_list,
        )
        .unwrap();
        time_rework_opti += now.elapsed().as_millis();
    }
    println!("Rework loop took: {:?}ms.", time_rework);
    println!("Rework opti loop took: {:?}ms.", time_rework_opti);


    println!(
        "Time to get the indices NEW: {:?} ms.",
        ((Instant::now() - now)/nb_loop).as_millis()
    );
    let mut new_unique_sigs = BasicVerifier::reworked_select_valid_signatures_for_k_indices(
        &closed_reg.total_stake,
        &params,
        &msgp,
        &sig_reg_list,
    )
    .unwrap();
    new_unique_sigs.sort_unstable();

    let now = Instant::now();
    for _ in 0..nb_loop {
        let _ = BasicVerifier::select_valid_signatures_for_k_indices(
            &closed_reg.total_stake,
            &params,
            &msgp,
            &sig_reg_list,
        )
        .unwrap();
    }
    println!(
        "Time to get the indices OLD: {:?} ms.",
        ((Instant::now() - now)/nb_loop).as_millis()
    );


}

fn main() {
    select_sig_test();
}