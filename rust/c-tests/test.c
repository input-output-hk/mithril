#include <stdio.h>
#include "../target/include/mithril.h"


int main(int argc, char **argv) {
    char *msg = argv[1];

#ifndef NEEDED_SIGS
#define NEEDED_SIGS 5
#endif

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

#ifdef FAIL
    int signeri = 1;
#else
    int signeri = 0;
#endif
    StmSignerPtr signer = stm_initializer_new_signer(initializer[signeri], 2, party_ids, party_stake, keys);

    int success = 0;
    for (Index i = 0; i < 100 && success < NEEDED_SIGS; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
            printf("Can sign index %lld\n", i);
            indices[success++] = i;
        }
    }

    if (success < NEEDED_SIGS) {
        printf("Not eligible to sign enough indices\n");
        return 1;
    }

    SigPtr sig[NEEDED_SIGS];
    for (int i = 0; i < NEEDED_SIGS; i++) {
        stm_signer_sign(signer, msg, indices[i], &sig[i]);
        if (!sig[i]) {
            printf("Signing signature %d failed\n", i);
            return 2;
        }
    }


    StmClerkPtr clerk = stm_clerk_from_signer(signer);
    for (int i = 0; i < NEEDED_SIGS; i++) {
        if (stm_clerk_verify_sig(clerk, sig[i], indices[i], msg)) {
            printf("Signature %d invalid\n", i);
            return 3;
        }
    }

    MultiSigPtr multi_sig;
    int r = stm_clerk_aggregate(clerk, NEEDED_SIGS, sig, indices, msg, &multi_sig);
    if (r || !multi_sig) {
        printf("Aggregation failed: ");
        if (r < 0) printf("Verification failed\n");
        else printf("Only got %d signatures\n", r);
        return 4;
    }

    int64_t msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    if (!msig_ok) {
        free_stm_clerk(clerk);
        for (int i = 0; i < NEEDED_SIGS; i++)
            free_sig(sig[i]);
        free_multi_sig((MultiSigPtr)multi_sig);
        printf("Test completed successfully!\n");
        return 0;
    } else {
        printf("Verification of multisignature failed\n");
        return 5;
    }
}
