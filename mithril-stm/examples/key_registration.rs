//! This example shows how Key Registration is held. It is not held by a single central party,
//! but instead by all the participants in the signature process. Contrarily to the full protocol
//! run presented in `tests/integration.rs`, we explicitly treat each party individually.
use blake2::{digest::consts::U32, Blake2b};
use mithril_stm::key_reg::{ClosedKeyReg, KeyReg};
use mithril_stm::participant::{StmInitializer, StmVerificationKeyPoP};
use mithril_stm::stm::{Stake, StmClerk, StmParameters};

use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = Blake2b<U32>;

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
    let stakes = (0..nparties)
        .map(|_| {
            let stake = rng.next_u64() % 999;
            total_stake += stake;
            1 + stake
        })
        .collect::<Vec<_>>();

    // Each party generates their Stm keys
    let party_0_init = StmInitializer::setup(params, stakes[0], &mut rng);
    let party_1_init = StmInitializer::setup(params, stakes[1], &mut rng);
    let party_2_init = StmInitializer::setup(params, stakes[2], &mut rng);
    let party_3_init = StmInitializer::setup(params, stakes[3], &mut rng);

    // The public keys are broadcast. All participants will have the same keys.
    let parties_pks: Vec<StmVerificationKeyPoP> = vec![
        party_0_init.verification_key(),
        party_1_init.verification_key(),
        party_2_init.verification_key(),
        party_3_init.verification_key(),
    ];

    // Now, each party generates their own KeyReg instance, and registers all other participating
    // parties. Once all parties are registered, the key registration is closed.
    let key_reg_0 = local_reg(&stakes, &parties_pks);
    let key_reg_1 = local_reg(&stakes, &parties_pks);
    let key_reg_2 = local_reg(&stakes, &parties_pks);
    let key_reg_3 = local_reg(&stakes, &parties_pks);

    // Now, with information of all participating parties (we can create the Merkle Tree), the
    // signers can be initialised.
    let party_0 = party_0_init.new_signer(key_reg_0).unwrap();
    let party_1 = party_1_init.new_signer(key_reg_1).unwrap();
    let party_2 = party_2_init.new_signer(key_reg_2).unwrap();
    let party_3 = party_3_init.new_signer(key_reg_3).unwrap();

    /////////////////////
    // operation phase //
    /////////////////////

    // Now an asynchronous phase begins. The signers no longer need to communicate among themselves
    // Given the parameters we've chosen, the signers will be eligible for all indices.
    let mut party_0_sigs = party_0
        .sign(&msg)
        .expect("Signers can sign all indices in this example");
    let mut party_1_sigs = party_1
        .sign(&msg)
        .expect("Signers can sign all indices in this example");
    let mut party_2_sigs = party_2
        .sign(&msg)
        .expect("Signers can sign all indices in this example");
    let mut party_3_sigs = party_3
        .sign(&msg)
        .expect("Signers can sign all indices in this example");

    // Parties must have signed all indices
    assert_eq!(party_0_sigs.indexes.len(), 10);
    assert_eq!(party_1_sigs.indexes.len(), 10);
    assert_eq!(party_2_sigs.indexes.len(), 10);
    assert_eq!(party_3_sigs.indexes.len(), 10);

    // Different combinations of signatures. Recall that we only need 3 signatures
    party_0_sigs.indexes = party_0_sigs.indexes[..2].to_vec();
    party_3_sigs.indexes = party_3_sigs.indexes[..2].to_vec();
    let complete_sigs_1 = vec![party_0_sigs.clone(), party_3_sigs.clone()];
    party_1_sigs.indexes = party_1_sigs.indexes[..5].to_vec();
    party_2_sigs.indexes = party_2_sigs.indexes[..5].to_vec();
    let complete_sigs_2 = vec![
        party_0_sigs.clone(),
        party_1_sigs.clone(),
        party_2_sigs.clone(),
        party_3_sigs.clone(),
    ];

    // The following is incomplete. While it has more than 3, it has a lot for the same index.
    party_1_sigs.indexes = party_1_sigs.indexes[..1].to_vec();
    party_2_sigs.indexes = party_2_sigs.indexes[..1].to_vec();
    party_3_sigs.indexes = party_3_sigs.indexes[..1].to_vec();
    let incomplete_sigs_3 = vec![party_0_sigs, party_1_sigs, party_2_sigs, party_3_sigs];

    let closed_registration = local_reg(&stakes, &parties_pks);
    let clerk = StmClerk::from_registration(&params, &closed_registration);

    // Now we aggregate the signatures
    let msig_1 = match clerk.aggregate(&complete_sigs_1, &msg) {
        Ok(s) => s,
        Err(e) => {
            panic!("Aggregation failed: {e:?}")
        }
    };
    assert!(msig_1.verify(&msg, &clerk.compute_avk(), &params).is_ok());

    let msig_2 = match clerk.aggregate(&complete_sigs_2, &msg) {
        Ok(s) => s,
        Err(e) => {
            panic!("Aggregation failed: {e:?}")
        }
    };
    assert!(msig_2.verify(&msg, &clerk.compute_avk(), &params).is_ok());

    let msig_3 = clerk.aggregate(&incomplete_sigs_3, &msg);
    assert!(msig_3.is_err());
}

fn local_reg(ids: &[u64], pks: &[StmVerificationKeyPoP]) -> ClosedKeyReg<H> {
    let mut local_keyreg = KeyReg::init();
    // data, such as the public key, stake and id.
    for (&pk, id) in pks.iter().zip(ids.iter()) {
        local_keyreg.register(*id, pk).unwrap();
    }
    local_keyreg.close()
}
