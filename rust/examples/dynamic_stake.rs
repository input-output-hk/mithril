//! In this example file, we present a scenario where the signers list and the stake
//! distribution is dynamic among different epochs. We show how instances should be
//! updated.

use ark_bls12_377::Bls12_377;
use blake2::Digest;
use mithril::key_reg::{ClosedKeyReg, KeyReg};
use mithril::msp::MspPk;
use mithril::stm::{Stake, StmInitializer, StmParameters};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = blake2::Blake2b;

fn main() {
    // Parameter. This information is broadcast (or known) to all
    // participants.
    let params = StmParameters {
        // Let's try with three signatures
        k: 3,
        m: 10,
        // `phi_f` = 1, so that it always passes, for the purpose of the example
        phi_f: 1.0,
    };

    println!("Parameters initialised: {:?}", params);

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

    ////////////////////////////////
    //////// EPOCH 1 ///////////////
    ////////////////////////////////

    println!("Begining of Epoch 1.");
    println!();
    // The example starts with only two parties.
    let nparties_e1 = 2;

    // We initialise the stake at epoch 1
    let mut total_stake_e1: Stake = 0;
    let parties_e1 = (0..nparties_e1)
        .into_iter()
        .map(|pid| {
            let stake = rng.next_u64() % 999;
            total_stake_e1 += stake;
            (pid, 1 + stake)
        })
        .collect::<Vec<_>>();

    println!("Total stake: {:?}", total_stake_e1);
    println!();

    // Each party generates their Stm keys
    let party_0_init_e1 = StmInitializer::setup(params, parties_e1[0].0, parties_e1[0].1, &mut rng);
    let party_1_init_e1 = StmInitializer::setup(params, parties_e1[1].0, parties_e1[1].1, &mut rng);

    // The public keys are broadcast. All participants will have the same keys. We expect
    // the keys to be persistent.
    let mut parties_pks: Vec<MspPk<Bls12_377>> = vec![
        party_0_init_e1.verification_key(),
        party_1_init_e1.verification_key(),
    ];
    println!("Parties initialised");
    println!(
        "Party 0: Stake => {:?}, PubKey => {:x}",
        parties_e1[0].1,
        H::digest(&parties_pks[0].mvk.to_bytes())
    );
    println!(
        "Party 1: Stake => {:?}, PubKey => {:x}",
        parties_e1[1].1,
        H::digest(&parties_pks[1].mvk.to_bytes())
    );
    println!();

    // Now, each party generates their own KeyReg instance, and registers all other participating
    // parties. Once all parties are registered, the key registration is closed.
    let party_0_key_reg_e1 = local_reg(&parties_e1, &parties_pks);
    let party_1_key_reg_e1 = local_reg(&parties_e1, &parties_pks);
    println!("Registration of epoch 1 is closed, and signers are initialised");
    println!();

    // Now, with information of all participating parties, the
    // signers can be initialised (we can create the Merkle Tree).
    let party_0 = party_0_init_e1.new_signer(&party_0_key_reg_e1);
    let party_1 = party_1_init_e1.new_signer(&party_1_key_reg_e1);
    println!("+------------------------+");
    println!("| Operation phase begins |");
    println!("| Signing happens here   |");
    println!("+------------------------+");

    //////////////////////////////////////
    ///////// OPERATION PHASE ////////////
    //////////////////////////////////////
    // Now we have a bunch of operation //
    // phases than happen. With the     //
    // same key and registration        //
    // instance, parties can sign       //
    // several messages.                //
    //////////////////////////////////////

    ////////////////////////////////
    //////// EPOCH 2 ///////////////
    ////////////////////////////////

    println!("Begining of Epoch 1.");
    println!();

    // Now the second epoch starts. A new party joins, and the stake of all
    // signers changes.
    let nparties_e2 = 3;

    // We initialise the stake at epoch 2
    let mut total_stake_e2: Stake = 0;
    let parties_e2 = (0..nparties_e2)
        .into_iter()
        .map(|pid| {
            let stake = rng.next_u64() % 999;
            total_stake_e2 += stake;
            (pid, 1 + stake)
        })
        .collect::<Vec<_>>();

    println!("Total stake: {:?}", total_stake_e2);
    println!();

    // Now the `StmSigner`s are outdated with respect to the new stake, and participants.
    // A way would be to create a fresh `StmInitializer` using the new stake, and then change
    // the keypair so that it corresponds to the keypair generated in Epoch 1. However,
    // we can simplify this by allowing a transition from `StmSigner` back to `StmInitializer`.
    // To decrease the chances of misusing this transition, the function not only consumes
    // the instance of `StmSigner`, but also that of the `ClosedKeyReg`.
    //
    // It would work as follows:
    let party_0_init_e2 = party_0.new_epoch(party_0_key_reg_e1, Some(parties_e2[0].1));
    let party_1_init_e2 = party_1.new_epoch(party_1_key_reg_e1, Some(parties_e2[1].1));

    // The third party needs to generate from scratch and broadcast the key (which we represent
    // by appending to the `pks` vector.
    let party_2_init_e2 = StmInitializer::setup(params, parties_e2[2].0, parties_e2[2].1, &mut rng);
    parties_pks.push(party_2_init_e2.verification_key());

    println!("Parties updated");
    println!(
        "Party 0: Stake => {:?}, PubKey => {:x}",
        parties_e2[0].1,
        H::digest(&parties_pks[0].mvk.to_bytes())
    );
    println!(
        "Party 1: Stake => {:?}, PubKey => {:x}",
        parties_e2[1].1,
        H::digest(&parties_pks[1].mvk.to_bytes())
    );
    println!(
        "Party 2: Stake => {:?}, PubKey => {:x}",
        parties_e2[2].1,
        H::digest(&parties_pks[2].mvk.to_bytes())
    );
    println!();

    // Now, the key reg of epoch 1 is consumed, so it cannot be used to generate a signer.
    // This forces us to re-run the key registration (which is good).
    let key_reg_e2 = vec![
        local_reg(&parties_e2, &parties_pks),
        local_reg(&parties_e2, &parties_pks),
        local_reg(&parties_e2, &parties_pks),
    ];
    println!("Registration of epoch 2 is closed, and signers are initialised");
    println!();
    // And finally, new signers can be created to signe messages in epoch 2. Again, signers
    // of epoch 1 are consumed, so they cannot be used to sign messages of this epoch (again,
    // this is good).
    let _party_0_e2 = party_0_init_e2.new_signer(&key_reg_e2[0]);
    let _party_1_e2 = party_1_init_e2.new_signer(&key_reg_e2[1]);
    let _party_2_e2 = party_2_init_e2.new_signer(&key_reg_e2[2]);
    println!("+------------------------+");
    println!("| Operation phase begins |");
    println!("| Signing happens here   |");
    println!("+------------------------+");
}

fn local_reg(ids: &[(usize, u64)], pks: &[MspPk<Bls12_377>]) -> ClosedKeyReg<Bls12_377, H> {
    let mut local_keyreg = KeyReg::new(ids);
    // todo: maybe its cleaner to have a `StmPublic` instance that covers the "shareable"
    // data, such as the public key, stake and id.
    for (&pk, id) in pks.iter().zip(ids.iter()) {
        match local_keyreg.register(id.0, pk) {
            Err(e) => panic!("{:?}", e),
            Ok(()) => (),
        }
    }
    local_keyreg.close()
}
