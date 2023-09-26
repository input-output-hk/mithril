mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        Beacon, Epoch, ProtocolParameters, SignedEntityType, SignedEntityTypeDiscriminants,
        StakeDistribution,
    },
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::{utilities::get_test_dir, ExpectedCertificate, RuntimeTester};

#[tokio::test]
async fn certificate_chain() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.95,
    };
    let configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("certificate_chain").join("aggregator.sqlite3"),
        ..Configuration::new_sample()
    };
    let mut tester =
        RuntimeTester::build(Beacon::new("net".to_string(), 1, 1), configuration).await;
    let observer = tester.observer.clone();

    comment!("Create signers & declare stake distribution");
    let initial_fixture = MithrilFixtureBuilder::default()
        .with_signers(5)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let signers = initial_fixture.signers_fixture();
    tester
        .init_state_from_fixture(&initial_fixture)
        .await
        .unwrap();
    let mut current_epoch = observer.current_epoch().await;

    comment!("Boostrap the genesis certificate, {:?}", current_epoch);
    tester
        .register_genesis_certificate(&initial_fixture)
        .await
        .unwrap();

    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(
            Beacon::new("devnet".to_string(), 1, 1),
            initial_fixture.compute_and_encode_avk()
        )
    );

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("Start the runtime state machine & send send first single signatures");
    cycle!(tester, "ready");
    cycle!(tester, "signing");
    tester.register_signers(&signers).await.unwrap();
    cycle_err!(tester, "signing");
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &signers,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 1, 2),
            &initial_fixture.signers_with_stake(),
            initial_fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!("The state machine should get back to signing to sign CardanoImmutableFilesFull");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            &signers,
        )
        .await
        .unwrap();
    comment!("The state machine should issue a certificate for the CardanoImmutableFilesFull");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 1, 3),
            &initial_fixture.signers_with_stake(),
            initial_fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoImmutableFilesFull(Beacon::new("devnet".to_string(), 1, 3)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!(
        "Increase immutable number to do a second CardanoImmutableFilesFull certificate for this epoch, {:?}",
        current_epoch
    );
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            &signers,
        )
        .await
        .unwrap();
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 1, 4),
            &initial_fixture.signers_with_stake(),
            initial_fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoImmutableFilesFull(Beacon::new("devnet".to_string(), 1, 4)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!("Change stake distribution");
    let next_fixture = {
        let updated_stake_distribution = StakeDistribution::from_iter(
            initial_fixture
                .signers_with_stake()
                .into_iter()
                .enumerate()
                .map(|(i, s)| (s.party_id, s.stake + (i as u64) * 1000)),
        );

        tester
            .update_stake_distribution(updated_stake_distribution)
            .await
            .unwrap()
    };
    let next_signers = next_fixture.signers_fixture();

    comment!(
        "Increase epoch, triggering stake distribution update, Next epoch: {:?}",
        current_epoch + 1
    );
    let new_epoch = tester.increase_epoch().await.unwrap();
    current_epoch = new_epoch;
    cycle!(tester, "idle");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!(
        "Checking that no signers are registered for the next epoch since they did not register"
    );
    let next_epoch_verification_keys = tester
        .dependencies
        .verification_key_store
        .get_verification_keys(new_epoch + 1)
        .await
        .expect("get_verification_keys should not fail");
    assert_eq!(
        None, next_epoch_verification_keys,
        "After a new epoch no signers should be registered for the next epoch"
    );

    comment!(
        "Signers register & send signatures, the new certificate should be link to the first of the previous epoch"
    );
    tester.register_signers(&next_signers).await.unwrap();
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &signers,
        )
        .await
        .unwrap();
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 2, 4),
            &initial_fixture.signers_with_stake(),
            initial_fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!(
        "Increase epoch & immutable, stake distribution updated at {} will now be signed into the certificate chain, allowing it to be used in the next epoch, Next epoch: {:?}",
        current_epoch,
        current_epoch + 1
    );
    current_epoch = tester.increase_epoch().await.unwrap();
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "idle");
    cycle!(tester, "ready");
    tester.register_signers(&signers).await.unwrap();
    cycle!(tester, "signing");

    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &signers,
        )
        .await
        .unwrap();
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 3, 5),
            &initial_fixture.signers_with_stake(),
            initial_fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(3)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(2))),
        )
    );

    comment!(
        "Increase epoch & immutable, stake distribution updated at {} should be signed in the new certificate, Next epoch: {:?}",
        current_epoch - 1,
        current_epoch + 1
    );
    current_epoch = tester.increase_epoch().await.unwrap();
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "idle");
    cycle!(tester, "ready");
    tester.register_signers(&signers).await.unwrap();
    cycle!(tester, "signing");

    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &next_signers,
        )
        .await
        .unwrap();
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 4, 6),
            &next_fixture.signers_with_stake(),
            next_fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(4)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(3))),
        )
    );

    comment!(
        "Increase immutable number to do a CardanoImmutableFilesFull certificate for this epoch, {:?}",
        current_epoch
    );
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");

    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            &next_signers,
        )
        .await
        .unwrap();
    cycle!(tester, "ready");

    comment!(
        "A CardanoImmutableFilesFull, linked to the MithrilStakeDistribution of the current epoch, should have been created, {:?}",
        current_epoch
    );
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 4, 7),
            &next_fixture.signers_with_stake(),
            next_fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoImmutableFilesFull(Beacon::new("devnet".to_string(), 4, 7)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(4))),
        )
    );
}
