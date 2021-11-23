#include <gtest/gtest.h>
extern "C" {
#include "../target/include/mithril.h"
}

#ifndef NEEDED_SIGS
#define NEEDED_SIGS 5
#endif

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

    // Scope of the signers, which is not required knowledge for the clerk. Signers initialise.
    {
        StmInitializerPtr initializer[2];

        for (int i = 0; i < 2; i++) {
            initializer[i] = stm_intializer_setup(params, party_ids[i], party_stake[i]);
            keys[i] = stm_initializer_verification_key(initializer[i]);
        }
        signer = stm_initializer_new_signer(initializer[0], 2, party_ids, party_stake, keys);
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
    close_registration(clerk_kr);
    // After closing the registration, the clerk can generate the global key
    MerkleTreePtr avk;
    ASSERT_EQ(generate_avk(clerk_kr, &avk), 0);
    StmClerkPtr clerk = stm_clerk_new(params, avk, 1);

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

    for (int i = 0; i < 2; i++) {
        initializer[i] = stm_intializer_setup(params, party_ids[i], party_stake[i]);
        keys[i] = stm_initializer_verification_key(initializer[i]);
    }

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

    StmSignerPtr signer = stm_initializer_new_signer(initializer[0], 2, party_ids, party_stake, keys);

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

    for (int i = 0; i < 2; i++) {
        initializer[i] = stm_intializer_setup(params, party_ids[i], party_stake[i]);
        keys[i] = stm_initializer_verification_key(initializer[i]);
    }

    StmSignerPtr signer = stm_initializer_new_signer(initializer[1], 2, party_ids, party_stake, keys);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
            indices[success++] = i;
        }
    }

    ASSERT_EQ(success, 0);
}
