mod test_extensions;

use mithril_aggregator::VerificationKeyStorer;
use mithril_common::crypto_helper::tests_setup;
use mithril_common::entities::{ProtocolParameters, SignerWithStake};
use test_extensions::RuntimeTester;

#[tokio::test]
async fn certificate_chain() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.65,
    };
    let mut tester = RuntimeTester::build(protocol_parameters.clone()).await;

    comment!("Create signers & declare stake distribution");
    let signers = tests_setup::setup_signers(5, &protocol_parameters.clone().into());
    let mut signers_with_stake: Vec<SignerWithStake> =
        signers.clone().into_iter().map(|s| s.into()).collect();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;
    tester
        .deps
        .simulate_genesis(
            signers_with_stake.clone(),
            signers_with_stake.clone(),
            &protocol_parameters,
        )
        .await;

    comment!("Increase epoch");
    tester.increase_epoch().await.unwrap();
    cycle!(tester, "idle");

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&signers).await.unwrap();

    comment!("Increase epoch");
    tester.increase_epoch().await.unwrap();
    cycle!(tester, "signing");

    comment!("Start the runtime state machine & send send first single signatures");
    cycle!(tester, "signing");
    tester.register_signers(&signers).await.unwrap();
    cycle!(tester, "signing");
    tester.send_single_signatures(&signers).await.unwrap();

    comment!("The state machine should have issued a multisignature");
    cycle!(tester, "idle");
    let (last_certificates, snapshots) =
        tester.get_last_certificates_and_snapshots().await.unwrap();
    assert_eq!((1, 1), (last_certificates.len(), snapshots.len()));
    cycle!(tester, "idle");

    comment!("Increase immutable number to do a second certificate for this epoch");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester.send_single_signatures(&signers).await.unwrap();
    cycle!(tester, "idle");
    let (last_certificates, snapshots) =
        tester.get_last_certificates_and_snapshots().await.unwrap();
    assert_eq!((2, 2), (last_certificates.len(), snapshots.len()));
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number + 1,
            last_certificates[1].beacon.epoch,
        ),
        "Only the immutable_file_number should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[1].hash,
        "A new certificate on the same epoch should be linked to the first certificate of the current epoch"
    );

    comment!("Change stake distribution");
    for (i, signer) in signers_with_stake.iter_mut().enumerate() {
        signer.stake += (i * 1000) as u64;
    }
    let new_signers = tester
        .update_stake_distribution(signers_with_stake)
        .await
        .unwrap();

    comment!("Increase epoch, triggering stake distribution update");
    let new_epoch = tester.increase_epoch().await.unwrap();
    cycle!(tester, "signing");

    comment!(
        "Checking that no signers are registered for the next epoch since they did not register"
    );
    let next_epoch_verification_keys = tester
        .deps
        .verification_key_store
        .get_verification_keys(new_epoch + 1)
        .await
        .expect("get_verification_keys should not fail");
    assert_eq!(
        None, next_epoch_verification_keys,
        "After a new epoch no signers should be registered for the next epoch"
    );

    comment!(
        "Signers register & send signatures, the new certificate should be link to the fist of the previous epoch"
    );
    tester.register_signers(&new_signers).await.unwrap();
    tester.send_single_signatures(&signers).await.unwrap();
    cycle!(tester, "idle");
    let (last_certificates, snapshots) =
        tester.get_last_certificates_and_snapshots().await.unwrap();
    assert_eq!((3, 3), (last_certificates.len(), snapshots.len()));
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number,
            last_certificates[1].beacon.epoch + 1,
        ),
        "Only the epoch should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[2].hash,
        "The new epoch certificate should be linked to the first certificate of the previous epoch"
    );
    assert_eq!(
        &last_certificates[0].metadata.get_stake_distribution(),
        &last_certificates[2].metadata.get_stake_distribution(),
        "The stake distribution update should only be take in account in the next epoch",
    );

    comment!(
        "Increase epoch & immutable, stake distribution updated previously should be signed in the new certificate"
    );
    tester.increase_epoch().await.unwrap();
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester.send_single_signatures(&signers).await.unwrap();
    cycle!(tester, "idle");

    // @todo:
    comment!("Why do we need a third epoch to use the new stake distribution ?");
    tester.increase_epoch().await.unwrap();
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester.send_single_signatures(&new_signers).await.unwrap();
    cycle!(tester, "idle");

    let (last_certificates, snapshots) =
        tester.get_last_certificates_and_snapshots().await.unwrap();
    assert_eq!((5, 5), (last_certificates.len(), snapshots.len()));
    assert_eq!(
        (
            last_certificates[0].beacon.immutable_file_number,
            last_certificates[0].beacon.epoch,
        ),
        (
            last_certificates[1].beacon.immutable_file_number + 1,
            last_certificates[1].beacon.epoch + 1,
        ),
        "Both the epoch & immutable file number should have changed"
    );
    assert_eq!(
        &last_certificates[0].previous_hash, &last_certificates[1].hash,
        "The new epoch certificate should be linked to the first certificate of the previous epoch"
    );
    assert_ne!(
        &last_certificates[0].metadata.get_stake_distribution(),
        &last_certificates[2].metadata.get_stake_distribution(),
        "The stake distribution update should have been applied for this epoch",
    );
}
