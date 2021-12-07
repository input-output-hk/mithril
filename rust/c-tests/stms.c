#include <gtest/gtest.h>
extern "C" {
#include "../target/include/mithril.h"
}

#ifndef NEEDED_SIGS
#define NEEDED_SIGS 5
#endif

/* Helper function to generate the key registration (and closure) for `nparties` number of parties. */
static void multiple_key_reg(const unsigned long nparties, PartyId *party_ids, Stake *party_stake, MspPkPtr *keys, ClosedKeyRegPtr *closed_reg) {
    KeyRegPtr key_regs[nparties];
    for (int i = 0; i < nparties; i++) {
        key_regs[i] = key_registration(nparties, party_ids, party_stake);
        // They register all parties (including themselves).
        for (int j = 0; j < nparties; j++) {
            ASSERT_EQ(register_party(key_regs[i], party_ids[j], keys[j]), 0);
        }
        // Now the registration phases closes, and no other parties can be included
        closed_reg[i] = close_registration(key_regs[i]);
    }
}

/* Helper function for the initialisation of the different parties. */
static void multiple_initializers(const unsigned long nparties, PartyId *party_ids, Stake *party_stake, StmParameters params, StmInitializerPtr *initializer, MspPkPtr *keys) {
    for (int i = 0; i < nparties; i++) {
        initializer[i] = stm_intializer_setup(params, party_ids[i], party_stake[i]);
        keys[i] = stm_initializer_verification_key(initializer[i]);
    }
}

TEST(stm, invalidRegistration) {
    // Lets test invalid registrations
    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[3] = {1, 2, 3};
    PartyId party_id_fake = 4;
    Stake   party_stake[3] = {1, 0, 0};
    Stake party_stake_fake = 4;
    MspPkPtr keys[3];
    MspPkPtr keys_fake;

    // Scope of the signers, which is not required knowledge for the clerk. Signers initialise.
    {
        StmInitializerPtr initializer[3];
        StmInitializerPtr initializer_fake;

        multiple_initializers(3, party_ids, party_stake, params, initializer, keys);
        initializer_fake = stm_intializer_setup(params, party_id_fake, party_stake_fake);
        keys_fake = stm_initializer_verification_key(initializer_fake);
    }

    // Now the public keys are broadcast, and a third party (i.e. a Clerk), can register all participating parties. However,
    // it registers all participants except the `fake`.
    KeyRegPtr clerk_kr = key_registration(3, party_ids, party_stake);
    ASSERT_EQ(register_party(clerk_kr, party_ids[0], keys[0]), 0);

    // If it register twice the same party, it fails with error code -1
    ASSERT_EQ(register_party(clerk_kr, party_ids[0], keys[0]), -1);

    // If the key is incorrect, then we get error code -2. For that, let's mangle the bits of a key
    unsigned char *fake_key;
    unsigned long size;
    msp_serialize_verification_key(keys[1], &size, &fake_key);
    fake_key[0] &= 0x00;
    MspPkPtr f_key = msp_deserialize_verification_key(size, fake_key);
    ASSERT_EQ(register_party(clerk_kr, party_ids[1], f_key), -2);

    // If we try to register the fake party, it will fail with error code -3
    ASSERT_EQ(register_party(clerk_kr, party_id_fake, keys_fake), -3);
}

TEST(stm, clerkFromPublicData) {
    // The following data is public, and known to all participants
    const char *msg = "some message";

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {1, 2};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmSignerPtr signer;

    // Scope of the signers, which is not required knowledge for the clerk. Signers initialise. There is no central
    // entity, so each signer needs to have its own instance of the key registration.
    {
        StmInitializerPtr initializer[2];
        ClosedKeyRegPtr closed_reg[2];

        multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
        multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);

        signer = stm_initializer_new_signer(initializer[0], closed_reg[0]);
    }

    // Now the public keys are broadcast, and a third party (i.e. a Clerk), can register all participating parties.

    // The Clerk first needs to run the registration with all participating parties. We explicitly list all steps.
    // First it defines which are the parties that can participate (this can happen before the clerk knows their
    // public keys).
    KeyRegPtr clerk_kr = key_registration(2, party_ids, party_stake);
    // Next it registers their public keys
    ASSERT_EQ(register_party(clerk_kr, party_ids[0], keys[0]), 0);
    ASSERT_EQ(register_party(clerk_kr, party_ids[1], keys[1]), 0);
    // Now the registration phases closes, and no other parties can be included
    ClosedKeyRegPtr closed_reg = close_registration(clerk_kr);
    // After closing the registration, the clerk can be created
    StmClerkPtr clerk = stm_clerk_from_reg(params, closed_reg);

    // The following can happen several times for distinct messaged, without requiring to run the registration phase
    // over again. Again, the following is out of scope of the Clerk. The latter only needs access to the resulting
    // signatures.
    SigPtr sig[NEEDED_SIGS];
    {
        int success = 0;
        for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
            if (stm_signer_eligibility_check(signer, msg, i)) {
                indices[success++] = i;
            }
        }

        for (int i = 0; i < NEEDED_SIGS; i++) {
            stm_signer_sign(signer, msg, indices[i], &sig[i]);
            ASSERT_NE(sig[i], nullptr);
        }
    }

    // Now, the Clerk can verify each signature individually.
    for (int i = 0; i < NEEDED_SIGS; i++) {
        ASSERT_EQ(stm_clerk_verify_sig(clerk, sig[i], indices[i], msg), 0);
    }

    // And finally, it aggregates them.
    MultiSigPtr multi_sig;
    int r = stm_clerk_aggregate(clerk, NEEDED_SIGS, sig, indices, msg, &multi_sig);
    ASSERT_EQ(r, 0);
    ASSERT_NE(multi_sig, nullptr);

    int64_t msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    ASSERT_EQ(msig_ok, 0);
    free_stm_clerk(clerk);
    for (int i = 0; i < NEEDED_SIGS; i++)
        free_sig(sig[i]);
    free_multi_sig((MultiSigPtr)multi_sig);
}

TEST(stm, produceAndVerifyAggregateSignature) {
    const char *msg = "some message";

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 101;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {1, 2};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
    // We just ensure that we can set the stake and params
    ASSERT_EQ(stm_initializer_stake(initializer[0]), party_stake[0]);

    stm_initializer_set_stake(initializer[0], 3);

    ASSERT_EQ(stm_initializer_stake(initializer[0]), 3);

    stm_initializer_set_stake(initializer[0], party_stake[0]);

    StmParameters new_params;
    new_params.k = NEEDED_SIGS;
    new_params.m = 100;
    new_params.phi_f = 0.2;

    stm_initializer_set_params(initializer[0], new_params);

    ASSERT_EQ(stm_initializer_params(initializer[0]).m, new_params.m);

    // Now , let's say that we store the secret key of the initialiser in (secure) memory.
    MspSkPtr sk = stm_initializer_secret_key(initializer[0]);

    // We can recover it later, after generating a fresh initializer. Given that the keys
    // have already been registered, a successful run of the protocol means that this key
    // recovery worked well.
    initializer[0] = stm_intializer_setup(params, party_ids[0], party_stake[0]);
    stm_initializer_set_keys(initializer[0], sk);

    // Each party needs to run its registration.
    multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);

    StmSignerPtr signer = stm_initializer_new_signer(initializer[0], closed_reg[0]);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
            indices[success++] = i;
        }
    }

    ASSERT_GE(success, NEEDED_SIGS);

    SigPtr sig[NEEDED_SIGS];
    for (int i = 0; i < NEEDED_SIGS; i++) {
        stm_signer_sign(signer, msg, indices[i], &sig[i]);
        ASSERT_NE(sig[i], nullptr);
    }

    StmClerkPtr clerk = stm_clerk_from_signer(signer);
    for (int i = 0; i < NEEDED_SIGS; i++) {
        ASSERT_EQ(stm_clerk_verify_sig(clerk, sig[i], indices[i], msg), 0);
    }

    MultiSigPtr multi_sig;
    int r = stm_clerk_aggregate(clerk, NEEDED_SIGS, sig, indices, msg, &multi_sig);
    ASSERT_EQ(r, 0);
    ASSERT_NE(multi_sig, nullptr);

    int64_t msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    ASSERT_EQ(msig_ok, 0);
    free_stm_clerk(clerk);
    for (int i = 0; i < NEEDED_SIGS; i++)
        free_sig(sig[i]);
    free_multi_sig((MultiSigPtr)multi_sig);
}

TEST(stm, failSigningIfIneligible) {
    const char *msg = "some message";

    StmParameters params;
    params.k = NEEDED_SIGS;
    params.m = 100;
    params.phi_f = 0.2;

    Index indices[NEEDED_SIGS];

    // Test with 2 parties, one with all the stake, one with none.
    PartyId party_ids[2] = {1, 2};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[2];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
    // Each party needs to run its registration.
    multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);

    StmSignerPtr signer = stm_initializer_new_signer(initializer[1], closed_reg[1]);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
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

    // Test with 2 parties, one with all the stake, one with none. In the second epoch we'll have three parties, so we
    // initialise the key array with a size of 3.
    PartyId party_ids[2] = {1, 2};
    Stake   party_stake[2] = {1, 0};
    MspPkPtr keys[3];
    StmInitializerPtr initializer[2];
    ClosedKeyRegPtr closed_reg[2];

    multiple_initializers(2, party_ids, party_stake, params, initializer, keys);
    multiple_key_reg(2, party_ids, party_stake, keys, closed_reg);

    StmSignerPtr signer[2];
    signer[0] = stm_initializer_new_signer(initializer[0], closed_reg[0]);
    signer[1] = stm_initializer_new_signer(initializer[1], closed_reg[1]);
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
    ASSERT_DEATH(stm_initializer_new_signer(initializer[1], closed_reg[1]), ".*");

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
    initializer_epoch2[2] = stm_intializer_setup(params, party_ids_epoch2[2], party_stake_epoch2[2]);
    keys[2] = stm_initializer_verification_key(initializer_epoch2[2]);

    // Given that the keys will be kept the same, the key registration can happen after the new party has broadcast its
    // key.
    multiple_key_reg(3, party_ids_epoch2, party_stake_epoch2, keys, closed_reg_epoch2);

    // In order to create the signer instance with the new registration, the previous signers need to create the
    // `StmInitializer` instance. They do so by consuming the signer and registration instances from epoch 1.
    initializer_epoch2[0] = stm_signer_new_epoch(signer[0], party_stake_epoch2[0]);
    initializer_epoch2[2] = stm_signer_new_epoch(signer[1], party_stake_epoch2[1]);

    // We can see that the stm_signer_new_epoch function frees the pointer
    ASSERT_DEATH(stm_signer_new_epoch(signer[1], party_stake_epoch2[1]), ".*");

    // Finally, the signer instances can be created for each signer, and an operation phase under the new stake
    // distribution may happen.
}
