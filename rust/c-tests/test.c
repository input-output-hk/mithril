#include <stdio.h>
#include "../target/include/mithril.h"

int main(int argc, char **argv) {
    char *msg = argv[1];

    StmParameters params;
    params.k = 1;
    params.m = 100;
    params.phi_f = 1.0;

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

    bool success = stm_signer_eligibility_check(signer, msg, 1);

    if (!success) {
        printf("Not eligible to sign\n");
        return 1;
    }

    SigPtr sig;
    stm_signer_sign(signer, msg, 1, &sig);

    if (!sig) {
        printf("Signing failed\n");
        return 2;
    }

    StmClerkPtr clerk = stm_clerk_from_signer(signer);
    
    if (stm_clerk_verify_sig(clerk, sig, 1, msg)) {
        printf("Signature invalid\n");
        return 3;
    }

    MultiSigConstPtr multi_sig;
    Index index = 1;
    int r = stm_clerk_aggregate(clerk, 1, sig, &index, msg, &multi_sig);
    if (r || !multi_sig) {
        printf("Aggregation failed: ");
        if (r < 0) printf("Verification failed\n");
        else printf("Only got %d signatures\n", r);
        return 4;
    }

    int64_t msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    if (!msig_ok) {
        free_stm_clerk(clerk);
        free_sig(sig);
        free_multi_sig((MultiSigPtr)multi_sig);
        printf("Test completed successfully!\n");
        return 0;
    } else {
        printf("Verification of multisignature failed\n");
        return 5;
    }
}
