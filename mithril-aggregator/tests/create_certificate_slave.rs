mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoDbBeacon, CardanoTransactionsSigningConfig, ChainPoint, Epoch,
        ProtocolParameters, SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber,
        StakeDistributionParty, TimePoint,
    },
    test_utils::{MithrilFixtureBuilder, TempDir},
};
use test_extensions::{
    utilities::get_test_dir, ExpectedCertificate, ExpectedMetrics, RuntimeTester,
};

#[tokio::test]
async fn create_certificate_slave() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let master_configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        signed_entity_types: Some(
            [
                SignedEntityTypeDiscriminants::CardanoTransactions.to_string(),
                SignedEntityTypeDiscriminants::CardanoDatabase.to_string(),
            ]
            .join(","),
        ),
        data_stores_directory: get_test_dir("create_certificate_master"),
        snapshot_directory: TempDir::create("aggregator-integration", "create_certificate_master"),
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(0),
            step: BlockNumber(30),
        },
        ..Configuration::new_sample()
    };
    let mut master_tester = RuntimeTester::build(
        TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint {
                slot_number: SlotNumber(10),
                block_number: BlockNumber(100),
                block_hash: "block_hash-100".to_string(),
            },
        },
        master_configuration,
    )
    .await;

    let slave_configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        signed_entity_types: Some(
            [
                SignedEntityTypeDiscriminants::CardanoTransactions.to_string(),
                SignedEntityTypeDiscriminants::CardanoDatabase.to_string(),
            ]
            .join(","),
        ),
        data_stores_directory: get_test_dir("create_certificate_slave"),
        snapshot_directory: TempDir::create("aggregator-integration", "create_certificate_slave"),
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(0),
            step: BlockNumber(30),
        },
        master_aggregator_endpoint: Some("http://localhost:8080".to_string()),
        ..Configuration::new_sample()
    };
    let mut slave_tester = RuntimeTester::build(
        TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint {
                slot_number: SlotNumber(10),
                block_number: BlockNumber(100),
                block_hash: "block_hash-100".to_string(),
            },
        },
        slave_configuration,
    )
    .await;

    comment!("** Epoch 1 **");

    comment!("Master: create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();

    master_tester
        .init_state_from_fixture(&fixture)
        .await
        .unwrap();

    comment!("Master: bootstrap the genesis certificate");
    master_tester
        .register_genesis_certificate(&fixture)
        .await
        .unwrap();

    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new_genesis(Epoch(1), fixture.compute_and_encode_avk())
    );

    comment!("Master: start the runtime state machine");
    cycle!(master_tester, "ready");
    cycle!(master_tester, "signing");

    comment!("Master: register signers");
    master_tester
        .register_signers(&fixture.signers_fixture())
        .await
        .unwrap();
    cycle_err!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new(
            Epoch(1),
            StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!("Master: change the epoch while signing");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

    comment!("** Epoch 2 **");

    comment!("Master: register signers");
    master_tester
        .register_signers(&fixture.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new(
            Epoch(2),
            StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    master_tester.expose_epoch_settings().await.unwrap();

    /*

    comment!("The state machine should get back to signing to sign CardanoImmutableFilesFull when a new immutable file exists");
    slave_tester.increase_immutable_number().await.unwrap();
    cycle!(slave_tester, "signing");
    let signers_for_immutables = &fixture.signers_fixture()[0..=6];
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            signers_for_immutables,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoImmutableFilesFull");
    cycle!(slave_tester, "ready");
    assert_last_certificate_eq!(
        slave_tester,
        ExpectedCertificate::new(
            Epoch(1),
            &signers_for_immutables
                .iter()
                .map(|s| s.signer_with_stake.clone().into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(1, 3)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!("The state machine should get back to signing to sign CardanoDatabase with the previously created immutable file");
    cycle!(slave_tester, "signing");
    let signers_for_cardano_database = &fixture.signers_fixture()[1..=6];
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoDatabase,
            signers_for_cardano_database,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoDatabase");
    cycle!(slave_tester, "ready");
    assert_last_certificate_eq!(
        slave_tester,
        ExpectedCertificate::new(
            Epoch(1),
            &signers_for_cardano_database
                .iter()
                .map(|s| s.signer_with_stake.clone().into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(1, 3)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!(
        "Increase cardano chain block number to 185,
        the state machine should be signing CardanoTransactions for block 179"
    );
    slave_tester
        .increase_block_number_and_slot_number(85, SlotNumber(95), BlockNumber(185))
        .await
        .unwrap();
    cycle!(slave_tester, "signing");
    let signers_for_transaction = &fixture.signers_fixture()[2..=6];
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            signers_for_transaction,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoTransactions");
    cycle!(slave_tester, "ready");
    assert_last_certificate_eq!(
        slave_tester,
        ExpectedCertificate::new(
            Epoch(1),
            &signers_for_transaction
                .iter()
                .map(|s| s.signer_with_stake.clone().into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoTransactions(Epoch(1), BlockNumber(179)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!(
        "Got rollback to block number 149 from cardano chain,
        the state machine should be signing CardanoTransactions for block 120"
    );
    slave_tester
        .cardano_chain_send_rollback(SlotNumber(95), BlockNumber(149))
        .await
        .unwrap();
    cycle!(slave_tester, "signing");
    let signers_for_transaction = &fixture.signers_fixture()[2..=6];
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            signers_for_transaction,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoTransactions");
    cycle!(slave_tester, "ready");
    assert_last_certificate_eq!(
        slave_tester,
        ExpectedCertificate::new(
            Epoch(1),
            &signers_for_transaction
                .iter()
                .map(|s| s.signer_with_stake.clone().into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoTransactions(Epoch(1), BlockNumber(119)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!("Change the epoch while signing");
    slave_tester.increase_immutable_number().await.unwrap();
    cycle!(slave_tester, "signing");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");

    cycle!(slave_tester, "ready");

    assert_metrics_eq!(
        slave_tester.metrics_verifier,
        ExpectedMetrics::new()
            .certificate_total(5)
            .artifact_cardano_immutable_files_full_total(1)
            .artifact_cardano_database_total(1)
            .artifact_mithril_stake_distribution_total(1)
            .artifact_cardano_transaction_total(2)
    ); */
}
