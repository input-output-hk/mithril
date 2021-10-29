//! This example shows how Key Registration is held. It is not held by a single central party,
//! but instead by all the participants in the signature process. Contrarily to the full protocol
//! run presented in `tests/integration.rs`, we explicitly treat each party individually.

use ark_bls12_377::Bls12_377;
use mithril::key_reg::KeyReg;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSigner};

use mithril::models::digest::DigestHash;
use mithril::msp::MspPk;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = blake2::Blake2b;

fn main() {
    let nparties = 4;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    // Parameter and parties initialisation. This information is broadcast (or known) to all
    // participants.
    let params = StmParameters {
        // Let's try with three signatures
        k: 3,
        m: 10,
        // so that it always passes
        phi_f: 1.0,
    };

    let parties = (0..nparties)
        .into_iter()
        .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
        .collect::<Vec<_>>();

    // Each party generates their Stm keys
    let party_0_init = StmInitializer::setup(params, parties[0].0, parties[0].1, &mut rng);
    let party_1_init = StmInitializer::setup(params, parties[1].0, parties[1].1, &mut rng);
    let party_2_init = StmInitializer::setup(params, parties[2].0, parties[2].1, &mut rng);
    let party_3_init = StmInitializer::setup(params, parties[3].0, parties[3].1, &mut rng);

    // The public keys are broadcast. All participants will have the same keys.
    let parties_pks: Vec<MspPk<Bls12_377>> = vec![
        party_0_init.verification_key(),
        party_1_init.verification_key(),
        party_2_init.verification_key(),
        party_3_init.verification_key(),
    ];

    // Now, each party generates their own KeyReg instance, and registers all other participating
    // parties
    // todo: if each party runs its own registration independently, this example fails. Figure out why.
    let key_reg_0 = local_reg(&parties, &parties_pks);
    let key_reg_1 = key_reg_0.clone(); // local_reg(&parties, &parties_pks); //
    let key_reg_2 = key_reg_0.clone(); // local_reg(&parties, &parties_pks); //
    let key_reg_3 = key_reg_0.clone(); // local_reg(&parties, &parties_pks); //

    // Now, with information of all participating parties (we can create the Merkle Tree), the
    // signers can be initialised.
    // todo: maybe we can directly pass `key_reg` to `StmInitialiser`, and do `retrieve_all` there
    let party_0 = party_0_init.new_signer(&key_reg_0.retrieve_all());
    let party_1 = party_1_init.new_signer(&key_reg_1.retrieve_all());
    let party_2 = party_2_init.new_signer(&key_reg_2.retrieve_all());
    let party_3 = party_3_init.new_signer(&key_reg_3.retrieve_all());

    /////////////////////
    // operation phase //
    /////////////////////

    // Now an asynchronous phase begins. The signers no longer need to communicate among themselves
    // Given the parameters we've chosen, the signers will be eligible for all indices.
    // todo: what if we have a structure that contains a signature and the index for which its valid?
    let (party_0_sigs, party_0_ixs) = try_signatures(&party_0, &msg, params.m);
    let (party_1_sigs, party_1_ixs) = try_signatures(&party_1, &msg, params.m);
    let (party_2_sigs, party_2_ixs) = try_signatures(&party_2, &msg, params.m);
    let (party_3_sigs, party_3_ixs) = try_signatures(&party_3, &msg, params.m);

    // Parties must have signed all indices
    assert_eq!(party_0_sigs.len(), 10);
    assert_eq!(party_1_sigs.len(), 10);
    assert_eq!(party_2_sigs.len(), 10);
    assert_eq!(party_3_sigs.len(), 10);

    // Different combinations of signatures. Recall that we only need 3 signatures
    let complete_sigs_1 = vec![
        party_0_sigs[0].clone(),
        party_0_sigs[1].clone(),
        party_3_sigs[2].clone(),
    ];
    let complete_ixs_1 = vec![
        party_0_ixs[0].clone(),
        party_0_ixs[1].clone(),
        party_3_ixs[2].clone(),
    ];

    let complete_sigs_2 = vec![
        party_1_sigs[0].clone(),
        party_2_sigs[1].clone(),
        party_0_sigs[2].clone(),
        party_1_sigs[3].clone(),
        party_3_sigs[4].clone(),
        party_1_sigs[5].clone(),
    ];
    let complete_ixs_2 = vec![
        party_1_ixs[0].clone(),
        party_2_ixs[1].clone(),
        party_0_ixs[2].clone(),
        party_1_ixs[3].clone(),
        party_3_ixs[4].clone(),
        party_1_ixs[5].clone(),
    ];

    // The following is incomplete. While it has more than 3, it has a lot for the same index.
    let incomplete_sigs_3 = vec![
        party_1_sigs[0].clone(),
        party_2_sigs[0].clone(),
        party_0_sigs[0].clone(),
        party_3_sigs[0].clone(),
        party_1_sigs[1].clone(),
    ];
    let incomplete_ixs_3 = vec![
        party_1_ixs[0].clone(),
        party_2_ixs[0].clone(),
        party_0_ixs[0].clone(),
        party_3_ixs[0].clone(),
        party_1_ixs[1].clone(),
    ];

    let clerk = StmClerk::from_signer(&party_0, TrivialEnv);

    // Now we aggregate the signatures
    let msig_1 =
        clerk.aggregate::<ConcatProof<Bls12_377, H>>(&complete_sigs_1, &complete_ixs_1, &msg);
    assert!(msig_1.is_ok());
    let aggr_1 = msig_1.unwrap();
    assert!(clerk.verify_msig::<ConcatProof<Bls12_377, H>>(&aggr_1, &msg));

    let msig_2 =
        clerk.aggregate::<ConcatProof<Bls12_377, H>>(&complete_sigs_2, &complete_ixs_2, &msg);
    assert!(msig_2.is_ok());
    let aggr_2 = msig_2.unwrap();
    assert!(clerk.verify_msig::<ConcatProof<Bls12_377, H>>(&aggr_2, &msg));

    let msig_3 =
        clerk.aggregate::<ConcatProof<Bls12_377, H>>(&incomplete_sigs_3, &incomplete_ixs_3, &msg);
    assert!(!msig_3.is_ok());
}

fn try_signatures(
    party: &StmSigner<H, Bls12_377>,
    msg: &[u8],
    m: u64,
) -> (Vec<StmSig<Bls12_377, DigestHash>>, Vec<u64>) {
    let mut sigs = Vec::new();
    let mut ixs = Vec::new();
    for ix in 0..m {
        if let Some(sig) = party.sign(msg, ix) {
            sigs.push(sig);
            ixs.push(ix);
        }
    }
    (sigs, ixs)
}

fn local_reg(ids: &Vec<(usize, u64)>, pks: &Vec<MspPk<Bls12_377>>) -> KeyReg<Bls12_377> {
    let mut local_keyreg = KeyReg::new(&ids);
    // todo: maybe its cleaner to have a `StmPublic` instance that covers the "shareable"
    // data, such as the public key, stake and id.
    for (&pk, id) in pks.into_iter().zip(ids.into_iter()) {
        assert!(local_keyreg.register(id.0, id.1, pk.clone()).is_ok());
    }
    local_keyreg
}
