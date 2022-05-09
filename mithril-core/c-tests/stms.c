#include <gtest/gtest.h>
extern "C" {
#include "../target/include/mithril.h"
}

#ifndef NEEDED_SIGS
#define NEEDED_SIGS 5
#endif

/* Helper function to generate the key registration (and closure) for `nparties` number of parties. */
static int multiple_key_reg(const unsigned long nparties, PartyId *party_ids, Stake *party_stake, MspPkPtr *keys, ClosedKeyRegPtr *closed_reg) {
    int err;
    KeyRegPtr key_regs[nparties];
    for (int i = 0; i < nparties; i++) {
        err = key_registration(nparties, party_ids, party_stake, &key_regs[i]);
        if (err != 0) {
            return err;
        }
        // They register all parties (including themselves).
        for (int j = 0; j < nparties; j++) {
            err = register_party(key_regs[i], party_ids[j], keys[j]);
            if (err != 0) {
                return err;
            }
        }
        // Now the registration phases closes, and no other parties can be included
        err = close_registration(key_regs[i], &closed_reg[i]);
        if (err != 0) {
            return err;
        }
    }
    return 0;
}

/* Helper function for the initialisation of the different parties. */
static int multiple_initializers(const unsigned long nparties, PartyId *party_ids, Stake *party_stake, StmParameters params, StmInitializerPtr *initializer, MspPkPtr *keys) {
    for (int i = 0; i < nparties; i++) {
        int err;
        err = stm_intializer_setup(params, party_ids[i], party_stake[i], &initializer[i]);
        if (err != 0) {
            return err;
        }
        err = stm_initializer_verification_key(initializer[i], &keys[i]);
        if (err != 0) {
            return err;
        }
    }
    return 0;
}

TEST(stm, invalidRegistration) {
    // Lets test invalid registrations
    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[3] = {0, 1, 2};
    PartyId party_id_fake = 3;
    Stake   party_stake[3] = {1, 0, 0};
    Stake party_stake_fake = 4;
    MspPkPtr keys[3];
    MspPkPtr keys_fake;

    int err;

    // Scope of the signers, which is not required knowledge for the clerk. Signers initialise.
    {
        StmInitializerPtr initializer[3];
        StmInitializerPtr initializer_fake;

        multiple_initializers(3, party_ids, party_stake, params, initializer, keys);
        err = stm_intializer_setup(params, party_id_fake, party_stake_fake, &initializer_fake);
        ASSERT_EQ(err, 0);
        err = stm_initializer_verification_key(initializer_fake, &keys_fake);
        ASSERT_EQ(err, 0);
    }

    // Now the public keys are broadcast, and a third party (i.e. a Clerk), can register all participating parties. However,
    // it registers all participants except the `fake`.
    KeyRegPtr clerk_kr;
    err = key_registration(3, party_ids, party_stake, &clerk_kr);
    ASSERT_EQ(err, 0);
    err = register_party(clerk_kr, party_ids[0], keys[0]);
    ASSERT_EQ(err, 0);

    // If it register twice the same party, it fails with error code -1
    err = register_party(clerk_kr, party_ids[0], keys[0]);
    ASSERT_EQ(err, -1);

    // todo: handle serialization
//    // If the key is incorrect, then we get error code -2. For that, let's mangle the bits of a key
//    unsigned char *fake_key;
//    unsigned long size;
//    err = msp_serialize_verification_key(keys[1], &size, &fake_key);
//    ASSERT_EQ(err, 0);
//    fake_key[0] &= 0x00;
//    MspPkPtr f_key;
//    err = msp_deserialize_verification_key(size, fake_key, &f_key);
//    ASSERT_EQ(err, 0);
//
//    err = register_party(clerk_kr, party_ids[1], f_key);
//    ASSERT_EQ(err, -2);

    // If we try to register the fake party, it will fail with error code -3
    err = register_party(clerk_kr, party_id_fake, keys_fake);
    ASSERT_EQ(err, -3);
}

TEST(stm, clerkFromPublicData) {
    // The following data is public, and known to all participants
    const char *msg = "some message";
    int err;

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {0, 1};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmSignerPtr signer;

    // Scope of the signers, which is not required knowledge for the clerk. Signers initialise. There is no central
    // entity, so each signer needs to have its own instance of the key registration.
    {
        StmInitializerPtr initializer[2];
        ClosedKeyRegPtr closed_reg[2];

        err = multiple_initializers(2, party_ids, party_stake, params, initializer, keys) ||
                multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);
        ASSERT_EQ(err, 0);

        err = stm_initializer_new_signer(initializer[0], closed_reg[0], &signer);
        ASSERT_EQ(err, 0);
    }

    // Now the public keys are broadcast, and a third party (i.e. a Clerk), can register all participating parties.

    // The Clerk first needs to run the registration with all participating parties. We explicitly list all steps.
    // First it defines which are the parties that can participate (this can happen before the clerk knows their
    // public keys).
    KeyRegPtr clerk_kr;
    err = key_registration(2, party_ids, party_stake, &clerk_kr);
    ASSERT_EQ(err, 0);
    // Next it registers their public keys
    err = register_party(clerk_kr, party_ids[0], keys[0]);
    ASSERT_EQ(err, 0);
    err = register_party(clerk_kr, party_ids[1], keys[1]);
    ASSERT_EQ(err, 0);
    // Now the registration phases closes, and no other parties can be included
    ClosedKeyRegPtr closed_reg;
    err = close_registration(clerk_kr, &closed_reg);
    ASSERT_EQ(err, 0);
    // After closing the registration, the clerk can be created
    StmClerkPtr clerk;
    err = stm_clerk_from_reg(params, closed_reg, &clerk);
    ASSERT_EQ(err, 0);

    // The following can happen several times for distinct messaged, without requiring to run the registration phase
    // over again. Again, the following is out of scope of the Clerk. The latter only needs access to the resulting
    // signatures.
    SigPtr sig[NEEDED_SIGS];
    {
        int success = 0;
        for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
            if (stm_signer_eligibility_check(signer, msg, i) == 0) {
                indices[success++] = i;
            }
        }

        for (int i = 0; i < NEEDED_SIGS; i++) {
            err = stm_signer_sign(signer, msg, indices[i], &sig[i]);
            ASSERT_EQ(err, 0);
            ASSERT_NE(sig[i], nullptr);
        }
    }

    // Now, the Clerk can verify each signature individually.
    for (int i = 0; i < NEEDED_SIGS; i++) {
        ASSERT_EQ(stm_clerk_verify_sig(clerk, sig[i], msg), 0);
    }

    // And finally, it aggregates them.
    MultiSigPtr multi_sig;
    err = stm_clerk_aggregate(clerk, NEEDED_SIGS, sig, msg, &multi_sig);
    ASSERT_EQ(err, 0);
    ASSERT_NE(multi_sig, nullptr);

    // Now we can generate the verifier (which does not need as much data as the clerk)
    StmVerifierPtr verifier;
    MerkleTreeCommitmentPtr avk_comm_ptr;
    Stake system_stake;

    generate_avk(closed_reg, &avk_comm_ptr);
    total_stake(closed_reg, &system_stake);
    err = stm_verifier_new(avk_comm_ptr, params, system_stake, &verifier);

    err = stm_verifier_verify_msig(verifier, multi_sig, msg);
    ASSERT_EQ(err, 0);
    free_stm_clerk(clerk);
    free_stm_verifier(verifier);
    for (int i = 0; i < NEEDED_SIGS; i++)
        free_sig(sig[i]);
    free_multi_sig((MultiSigPtr)multi_sig);
    free_closed_reg(closed_reg);
}

TEST(stm, produceAndVerifyAggregateSignature) {
    const char *msg = "some message";
    int err;

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {5, 134};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    ASSERT_EQ(multiple_initializers(2, party_ids, party_stake, params, initializer, keys), 0);
    // We just ensure that we can set the stake and params
    Stake retrieved_stake;
    err = stm_initializer_stake(initializer[0], &retrieved_stake);
    ASSERT_EQ(err, 0);
    ASSERT_EQ(retrieved_stake, party_stake[0]);

    err = stm_initializer_set_stake(initializer[0], 3);
    ASSERT_EQ(err, 0);

    err = stm_initializer_stake(initializer[0], &retrieved_stake);
    ASSERT_EQ(retrieved_stake, 3);

    err = stm_initializer_set_stake(initializer[0], party_stake[0]);
    ASSERT_EQ(err, 0);

    StmParameters new_params;
    new_params.k = NEEDED_SIGS;
    new_params.m = 100;
    new_params.phi_f = 0.2;

    err = stm_initializer_set_params(initializer[0], new_params);
    ASSERT_EQ(err, 0);

    StmParameters retrieved_params;
    err = stm_initializer_params(initializer[0], &retrieved_params);
    ASSERT_EQ(err, 0);
    ASSERT_EQ(retrieved_params.m, new_params.m);

    // Each party needs to run its registration.
    err = multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);
    ASSERT_EQ(err, 0);

    StmSignerPtr signer;
    err = stm_initializer_new_signer(initializer[0], closed_reg[0], &signer);
    ASSERT_EQ(err, 0);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i) == 0) {
            indices[success++] = i;
        }
    }

    ASSERT_GE(success, NEEDED_SIGS);

    SigPtr sig[NEEDED_SIGS];
    for (int i = 0; i < NEEDED_SIGS; i++) {
        err = stm_signer_sign(signer, msg, indices[i], &sig[i]);
        ASSERT_EQ(err, 0);
        ASSERT_NE(sig[i], nullptr);
    }

    StmClerkPtr clerk;
    err = stm_clerk_from_signer(signer, &clerk);
    ASSERT_EQ(err, 0);
    for (int i = 0; i < NEEDED_SIGS; i++) {
        ASSERT_EQ(stm_clerk_verify_sig(clerk, sig[i], msg), 0);
    }

    MultiSigPtr multi_sig;
    err = stm_clerk_aggregate(clerk, NEEDED_SIGS, sig, msg, &multi_sig);
    ASSERT_EQ(err, 0);
    ASSERT_NE(multi_sig, nullptr);

    err = stm_clerk_verify_msig(clerk, multi_sig, msg);
    ASSERT_EQ(err, 0);
    free_stm_clerk(clerk);
    for (int i = 0; i < NEEDED_SIGS; i++)
        free_sig(sig[i]);
    free_multi_sig((MultiSigPtr)multi_sig);
}

TEST(stm, failSigningIfIneligible) {
    const char *msg = "some message";
    int err;

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 100;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {0, 1};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    err = multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
    ASSERT_EQ(err, 0);
    // Each party needs to run its registration.
    err = multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);
    ASSERT_EQ(err, 0);

    StmSignerPtr signer;
    err = stm_initializer_new_signer(initializer[1], closed_reg[1], &signer);
    ASSERT_EQ(err, 0);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i) == 0) {
            indices[success++] = i;
        }
    }

    ASSERT_EQ(success, 0);
}

TEST(stm, dynamicStake) {
    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 100;
    params.phi_f = 0.2;
    int err;

    // Test with 2 parties, one with all the stake, one with none. In the second epoch we'll have three parties, so we
    // initialise the key array with a size of 3.
    PartyId party_ids[2] = {5, 13};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[3];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    err = multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
    ASSERT_EQ(err, 0);
    err = multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);
    ASSERT_EQ(err, 0);

    StmSignerPtr signer[2];
    err = stm_initializer_new_signer(initializer[0], closed_reg[0], &signer[0]);
    ASSERT_EQ(err, 0);
    err = stm_initializer_new_signer(initializer[1], closed_reg[1], &signer[1]);
    ASSERT_EQ(err, 0);
    /*
     * Some signing happens. This is the operation phase. At some point the operation phase finishes, and we proceed to
     * the next epoch. In this epoch change, the stake and the parties are updated. This means that we need to
     * update the signer (as it contains the merkle tree root, and the total stake). One way to go would be to
     * extract the key pair from the initializer, regenerate a fresh one with the new params, and change the key
     * pair of the fresh initializer with the key pair of the previous one. However, that is not the best way.
     * Particularly because the `stm_initializer_new_signer` function consumes the initializer and registration. This
     * is to ensure that one cannot change the initializer values once the key registration is closed. We can see that
     * the pointer was already freed.
     */
    // todo: does it make sense to free the pointer?
    ASSERT_DEATH(stm_initializer_new_signer(initializer[1], closed_reg[1], &signer[1]), ".*");

    /*
     * Therefore, what we need to do is go back from the signer to an initializer instance. To minimise possible misuse
     * of the transition, we consume the closed registration, so that we force the library user to regenerate a new
     * registration instance (with the updated parameters). Let's also assume that there is a new signer.
     */
    PartyId party_ids_epoch2[3] = {1, 2, 3};
    Stake   party_stake_epoch2[3] = {0, 1, 0};
    StmInitializerPtr initializer_epoch2[3];
    ClosedKeyRegPtr closed_reg_epoch2[3];

    // Only party 2 needs to create its initializer
    err = stm_intializer_setup(params, party_ids_epoch2[2], party_stake_epoch2[2], &initializer_epoch2[2]);
    ASSERT_EQ(err, 0);
    err = stm_initializer_verification_key(initializer_epoch2[2], &keys[2]);
    ASSERT_EQ(err, 0);

    // Given that the keys will be kept the same, the key registration can happen after the new party has broadcast its
    // key.
    err = multiple_key_reg(3, party_ids_epoch2, party_stake_epoch2, keys, closed_reg_epoch2);
    ASSERT_EQ(err, 0);

    // In order to create the signer instance with the new registration, the previous signers need to create the
    // `StmInitializer` instance. They do so by consuming the signer and registration instances from epoch 1.
    err = stm_signer_new_epoch(signer[0], party_stake_epoch2[0], &initializer_epoch2[0]);
    ASSERT_EQ(err, 0);
    err = stm_signer_new_epoch(signer[1], party_stake_epoch2[1], &initializer_epoch2[1]);
    ASSERT_EQ(err, 0);

    // We can see that the stm_signer_new_epoch function frees the pointer
    ASSERT_DEATH(stm_signer_new_epoch(signer[1], party_stake_epoch2[1], &initializer_epoch2[1]), ".*");

    // Finally, the signer instances can be created for each signer, and an operation phase under the new stake
    // distribution may happen.
}
