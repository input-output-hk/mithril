//! This example shows how Key Registration is held. It is not held by a single central party,
//! but instead by all the participants in the signature process. Contrarily to the full protocol
//! run presented in `tests/integration.rs`, we explicitly treat each party individually.

use mithril::key_reg::{ClosedKeyReg, KeyReg};
use mithril::stm::{
    Stake, StmClerk, StmInitializer, StmParameters, StmSig, StmSigner, StmVerifier,
};

use mithril::msp::VerificationKeyPoP;
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
        // `phi_f` = 1, so that it always passes, for the purpose of the example
        phi_f: 1.0,
    };

    let mut total_stake: Stake = 0;
    let parties = (0..nparties)
        .into_iter()
        .map(|pid| {
            let stake = rng.next_u64() % 999;
            total_stake += stake;
            (pid, 1 + stake)
        })
        .collect::<Vec<_>>();

    // Each party generates their Stm keys
    let party_0_init = StmInitializer::setup(params, parties[0].0, parties[0].1, &mut rng);
    let party_1_init = StmInitializer::setup(params, parties[1].0, parties[1].1, &mut rng);
    let party_2_init = StmInitializer::setup(params, parties[2].0, parties[2].1, &mut rng);
    let party_3_init = StmInitializer::setup(params, parties[3].0, parties[3].1, &mut rng);

    // The public keys are broadcast. All participants will have the same keys.
    let parties_pks: Vec<VerificationKeyPoP> = vec![
        party_0_init.verification_key(),
        party_1_init.verification_key(),
        party_2_init.verification_key(),
        party_3_init.verification_key(),
    ];

    // Now, each party generates their own KeyReg instance, and registers all other participating
    // parties. Once all parties are registered, the key registration is closed.
    let key_reg_0 = local_reg(&parties, &parties_pks);
    let key_reg_1 = local_reg(&parties, &parties_pks);
    let key_reg_2 = local_reg(&parties, &parties_pks);
    let key_reg_3 = local_reg(&parties, &parties_pks);

    // Now, with information of all participating parties (we can create the Merkle Tree), the
    // signers can be initialised.
    let party_0 = party_0_init.new_signer(key_reg_0);
    let party_1 = party_1_init.new_signer(key_reg_1);
    let party_2 = party_2_init.new_signer(key_reg_2);
    let party_3 = party_3_init.new_signer(key_reg_3);

    /////////////////////
    // operation phase //
    /////////////////////

    // Now an asynchronous phase begins. The signers no longer need to communicate among themselves
    // Given the parameters we've chosen, the signers will be eligible for all indices.
    // todo: what if we have a structure that contains a signature and the index for which its valid?
    let party_0_sigs = try_signatures(&party_0, &msg, params.m);
    let party_1_sigs = try_signatures(&party_1, &msg, params.m);
    let party_2_sigs = try_signatures(&party_2, &msg, params.m);
    let party_3_sigs = try_signatures(&party_3, &msg, params.m);

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

    let complete_sigs_2 = vec![
        party_1_sigs[0].clone(),
        party_2_sigs[1].clone(),
        party_0_sigs[2].clone(),
        party_1_sigs[3].clone(),
        party_3_sigs[4].clone(),
        party_1_sigs[5].clone(),
    ];

    // The following is incomplete. While it has more than 3, it has a lot for the same index.
    let incomplete_sigs_3 = vec![
        party_1_sigs[0].clone(),
        party_2_sigs[0].clone(),
        party_0_sigs[0].clone(),
        party_3_sigs[0].clone(),
        party_1_sigs[1].clone(),
    ];

    let closed_registration = local_reg(&parties, &parties_pks);
    let clerk = StmClerk::from_registration(params, closed_registration.clone());
    let verifier = StmVerifier::new(
        closed_registration.avk.to_commitment(),
        params,
        closed_registration.total_stake,
    );

    // Now we aggregate the signatures
    let msig_1 = match clerk.aggregate(&complete_sigs_1, &msg) {
        Ok(s) => s,
        Err(e) => {
            panic!("Aggregation failed: {:?}", e)
        }
    };
    assert!(verifier.verify_msig(&msg, &msig_1).is_ok());

    let msig_2 = match clerk.aggregate(&complete_sigs_2, &msg) {
        Ok(s) => s,
        Err(e) => {
            panic!("Aggregation failed: {:?}", e)
        }
    };
    assert!(verifier.verify_msig(&msg, &msig_2).is_ok());

    let msig_3 = clerk.aggregate(&incomplete_sigs_3, &msg);
    assert!(msig_3.is_err());
}

fn try_signatures(party: &StmSigner<H>, msg: &[u8], m: u64) -> Vec<StmSig<H>> {
    (0..m)
        .into_iter()
        .filter_map(|ix| party.sign(msg, ix))
        .collect()
}

fn local_reg(ids: &[(u64, u64)], pks: &[VerificationKeyPoP]) -> ClosedKeyReg<H> {
    let mut local_keyreg = KeyReg::new(ids);
    // todo: maybe its cleaner to have a `StmPublic` instance that covers the "shareable"
    // data, such as the public key, stake and id.
    for (&pk, id) in pks.iter().zip(ids.iter()) {
        local_keyreg.register(id.0, pk).unwrap();
    }
    local_keyreg.close()
}
