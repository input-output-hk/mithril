#include <stdio.h>
#include "../include/mithril.h"

int main(int argc, char **argv) {
    char *msg = argv[1];

    StmParameters params;
    params.k = 1;
    params.m = 100;
    params.phi_f = 1.0;

    Participant p0;
    p0.stake = 1;
    p0.party_id = 1;

    KeyRegPtr key_reg = key_reg_new(1, &p0);

    StmInitializerPtr initializer = stm_intializer_setup(params, 1, 1);
    stm_initializer_register(initializer, key_reg);
    stm_initializer_build_avk(initializer, key_reg);
    free_keyreg(key_reg);

    StmSignerPtr signer = stm_initializer_finish(initializer);
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
    if (!stm_clerk_verify_sig(clerk, sig, 1, msg)) {
        printf("Signature invalid\n");
        return 3;
    }

    MultiSigConstPtr multi_sig;
    Index index = 1;
    int r = stm_clerk_aggregate(clerk, 1, sig, &index, msg, &multi_sig);
    if (r) {
        printf("Aggregation failed: ");
        if (r < 0) printf("Verification failed\n");
        else printf("Only got %d signatures\n", r);
        return 4;
    }

    bool msig_ok = stm_clerk_verify_msig(clerk, multi_sig, msg);
    if (msig_ok) {
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
