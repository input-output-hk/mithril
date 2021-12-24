#include <gtest/gtest.h>
extern "C" {
#include "../target/include/mithril.h"
}

TEST(atm, produceAndVerifyAggregateSignature) {
    const char *msg = "some message";

    // Test with 5 parties, each with stake = 1, which is equivalent to a setting where one signer has one
    // vote. And let the threshold be 4.
    PartyId party_ids[5] = {1, 2, 3, 4, 5};
    Stake party_stake[5] = {1, 1, 1, 1, 1};
    MspPkPtr keys[5];
    MspSkPtr sks[5];
    unsigned int nr_signers = 5;
    unsigned int threshold = 4;
    int err;

    for (int i = 0; i < nr_signers; i++) {
        err = msp_generate_keypair(&sks[i], &keys[i]);
        ASSERT_EQ(err, 0);
    }

    // The threshold public key can be generated by using the public key parts of the participants.
    AvkPtr avk_pk;

    err = avk_key_aggregation(keys, party_stake, nr_signers, threshold, &avk_pk);
    ASSERT_EQ(err, 0);

    // Now, signers can produce threshold signatures with respect to `avk_pk`. Now, let's assume that
    // parties 0-3 generate a signature.
    MspSigPtr sigs[4];

    for (int i = 0; i < 4; i++) {
         err = msp_sign(msg, sks[i], &sigs[i]);
        ASSERT_EQ(err, 0);
    }

    // Given the four signatures and the public keys from the corresponding signers, any third party (not necessarily
    // a signer) can aggregate the signatures. It only needs knowledge of the avk key over which it is supposed to be
    // valid.
    AsigPtr aggregated_sig;
    err = atms_aggregate_sigs(sigs, keys, avk_pk, 4, &aggregated_sig);
    ASSERT_EQ(err, 0);

    // Finally, we check that the signature is indeed valid.
    err = atms_verify_sig(msg, aggregated_sig, avk_pk);
    ASSERT_EQ(err, 0);
}

TEST(atm, testingErrors) {
    const char *msg = "some message";

    // Test with 5 parties and set the threshold to 4.
    PartyId party_ids[5] = {1, 2, 3, 4, 5};
    Stake party_stake[5] = {1, 1, 1, 1, 1};
    MspPkPtr keys[5];
    MspSkPtr sks[5];
    unsigned int nr_signers = 5;
    unsigned int threshold = 3;
    int err;

    for (int i = 0; i < nr_signers; i++) {
        err = msp_generate_keypair(&sks[i], &keys[i]);
        ASSERT_EQ(err, 0);
    }

    // The threshold public key can be generated by using the public key parts of the participants. We leave out of this
    // the key of the "fake" party.
    AvkPtr avk_pk;
    err = avk_key_aggregation(keys, party_stake, nr_signers, threshold, &avk_pk);
    ASSERT_EQ(err, 0);

    // Now, signers can produce threshold signatures with respect to `avk_pk`. Now, let's assume that
    // only 0-3 generate a signature.
    MspSigPtr sigs[3];

    for (int i = 0; i < 3; i++) {
        err = msp_sign(msg, sks[i], &sigs[i]);
        ASSERT_EQ(err, 0);
    }

    // The signature is valid on its own
    err = msp_verify(msg, keys[0], sigs[0]);
    ASSERT_EQ(err, 0);

    // And the signature is invalid when verified over an invalid key
    err = msp_verify(msg, keys[1], sigs[0]);
    ASSERT_EQ(err, -1);

    // First we create a signature with no sufficient signers.
    AsigPtr aggregated_sig_1;
    err = atms_aggregate_sigs(sigs, keys, avk_pk, 2, &aggregated_sig_1);
    ASSERT_EQ(err, 0);
    err = atms_verify_sig(msg, aggregated_sig_1, avk_pk);
    ASSERT_EQ(err, -1);

    // We also create keys which will not be registered
    MspPkPtr fake_keys[3];
    MspSkPtr fake_skeys[3];

    for (int i = 0; i < 3; i++) {
        err = msp_generate_keypair(&fake_skeys[i], &fake_keys[i]);
        ASSERT_EQ(err, 0);
    }

    // We also create a "fake" avk;
    AvkPtr avk_pk_fake;
    err = avk_key_aggregation(fake_keys, party_stake, 3, 2, &avk_pk_fake);
    ASSERT_EQ(err, 0);

    // Now, fake signature
    MspSigPtr fake_sigs[2];

    for (int i = 0; i < 2; i++) {
        err = msp_sign(msg, fake_skeys[i], &fake_sigs[i]);
        ASSERT_EQ(err, 0);
    }

    // Now we create a signature with fake_key and verify it with a different key. It will fail because the non-signers
    // are not found in the verification key `avk_pk`. Error message should be -4.
    AsigPtr aggregated_sig_2;
    err = atms_aggregate_sigs(fake_sigs, fake_keys, avk_pk_fake, 2, &aggregated_sig_2);
    ASSERT_EQ(err, 0);
    err = atms_verify_sig(msg, aggregated_sig_2, avk_pk);
    ASSERT_EQ(err, -4);

    // If instead there are no `non-signers` (all signers submit a signature), but the signature is invalid, the error
    // message is -5, as the error now is simply that verification failed.
    MspPkPtr fake_key;
    MspSkPtr fake_skey;
    err = msp_generate_keypair(&fake_skey, &fake_key);
    ASSERT_EQ(err, 0);
    AvkPtr single_aggr_key;
    err = avk_key_aggregation(&fake_key, party_stake, 1, 1, &single_aggr_key);
    ASSERT_EQ(err, 0);
    MspSigPtr fake_sig;
    err = msp_sign(msg, fake_skey, &fake_sig);
    ASSERT_EQ(err, 0);

    AsigPtr aggregated_sig_3;
    err = atms_aggregate_sigs(&fake_sig, &fake_key, single_aggr_key, 1, &aggregated_sig_3);
    ASSERT_EQ(err, 0);
    err = atms_verify_sig(msg, aggregated_sig_3, avk_pk);
    ASSERT_EQ(err, -5);
}
