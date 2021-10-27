#include <stdio.h>
#include "../target/include/mithril.h"


int main(int argc, char **argv) {
    char *msg = argv[1];

    StmParameters params;
    params.k = 2;
    params.m = 100;
    params.phi_f = 0.2;

    Index indices[2];
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
    for (Index i = 0; i < 100 && success < 2; i++) {
        if (stm_signer_eligibility_check(signer, msg, i)) {
            printf("Can sign index %llx\n", i);
            indices[success++] = i;
        }
    }

    if (success < 2) {
        printf("Not eligible to sign\n");
        return 1;
    }

    SigPtr sig[2];
    stm_signer_sign(signer, msg, indices[0], &sig[0]);
    stm_signer_sign(signer, msg, indices[1], &sig[1]);

    if (!sig[0] || !sig[1]) {
        printf("Signing failed\n");
        return 2;
    }

    StmClerkPtr clerk = stm_clerk_from_signer(signer);
    if (stm_clerk_verify_sig(clerk, sig[0], indices[0], msg)) {
        printf("Signature 1 invalid\n");
        return 3;
    }
    if (stm_clerk_verify_sig(clerk, sig[1], indices[1], msg)) {
        printf("Signature 2 invalid\n");
        return 3;
    }

    MultiSigPtr multi_sig;
    int r = stm_clerk_aggregate(clerk, 2, sig, indices, msg, &multi_sig);
    if (r || !multi_sig) {
        printf("Aggregation failed: ");
        if (r < 0) printf("Verification failed\n");
        else printf("Only got %d signatures\n", r);
        return 4;
    }

    int64_t msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    if (!msig_ok) {
        free_stm_clerk(clerk);
        free_sig(sig[0]);
        free_sig(sig[1]);
        free_multi_sig((MultiSigPtr)multi_sig);
        printf("Test completed successfully!\n");
        return 0;
    } else {
        printf("Verification of multisignature failed\n");
        return 5;
    }
}
