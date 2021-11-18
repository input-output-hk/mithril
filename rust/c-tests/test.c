#include <gtest/gtest.h>
extern "C" {
#include "../target/include/mithril.h"
}

TEST(stm, ok) {
    const char *msg = "some message";

#ifndef NEEDED_SIGS
#define NEEDED_SIGS 5
#endif

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

    if (stm_initializer_stake(initializer[0]) != party_stake[0]) {
        printf("Stake check failed");
        return 1;
    }

    stm_initializer_set_stake(initializer[0], 3);

    if (stm_initializer_stake(initializer[0]) != 3) {
        printf("Update stake failed");
        return 1;
    }

    StmParameters new_params;
    new_params.k = NEEDED_SIGS;
    new_params.m = 100;
    new_params.phi_f = 0.2;

    stm_initializer_set_params(initializer[0], new_params);

    if (stm_initializer_params(initializer[0]).m != new_params.m) {
        printf("Failed to update parameters");
        return 1;
    }

#ifdef FAIL
    int signeri = 1;
#else
    int signeri = 0;
#endif
    StmSignerPtr signer = stm_initializer_new_signer(initializer[signeri], 2, party_ids, party_stake, keys);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
            printf("Can sign index %ld\n", i);
            indices[success++] = i;
        }
    }

    ASSERT_LT(success, NEEDED_SIGS);

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
