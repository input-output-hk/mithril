mod test_extensions;

use mithril_aggregator::ServeCommandConfiguration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoDbBeacon, CardanoTransactionsSigningConfig, ChainPoint, Epoch,
        ProtocolParameters, SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber,
        StakeDistributionParty, TimePoint,
    },
    temp_dir,
    test::builder::MithrilFixtureBuilder,
};
use test_extensions::{
    ExpectedCertificate, ExpectedMetrics, RuntimeTester, utilities::get_test_dir,
};

#[tokio::test]
async fn create_certificate() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let configuration = ServeCommandConfiguration {
        protocol_parameters: protocol_parameters.clone(),
        signed_entity_types: Some(
            [
                SignedEntityTypeDiscriminants::CardanoTransactions.to_string(),
                SignedEntityTypeDiscriminants::CardanoDatabase.to_string(),
            ]
            .join(","),
        ),
        data_stores_directory: get_test_dir("create_certificate"),
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(0),
            step: BlockNumber(30),
        },
        ..ServeCommandConfiguration::new_sample(temp_dir!())
    };
    let mut tester = RuntimeTester::build(
        TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint {
                slot_number: SlotNumber(10),
                block_number: BlockNumber(100),
                block_hash: "block_hash-100".to_string(),
            },
        },
        configuration,
    )
    .await;

    comment!("create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();

    tester.init_state_from_fixture(&fixture).await.unwrap();

    comment!("Bootstrap the genesis certificate");
    tester.register_genesis_certificate(&fixture).await.unwrap();

    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(Epoch(1), fixture.compute_and_encode_avk())
    );

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("start the runtime state machine");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!("register signers");
    tester.register_signers(&fixture.signers_fixture()).await.unwrap();
    cycle_err!(tester, "signing");

    comment!("signers send their single signature");
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Epoch(1),
            StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    comment!(
        "The state machine should get back to signing to sign CardanoDatabase with the previously created immutable file"
    );
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    let signers_for_cardano_database = &fixture.signers_fixture()[1..=6];
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoDatabase,
            signers_for_cardano_database,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoDatabase");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
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
    tester
        .increase_block_number_and_slot_number(85, SlotNumber(95), BlockNumber(185))
        .await
        .unwrap();
    cycle!(tester, "signing");
    let signers_for_transaction = &fixture.signers_fixture()[2..=6];
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            signers_for_transaction,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoTransactions");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
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
    tester
        .cardano_chain_send_rollback(SlotNumber(95), BlockNumber(149))
        .await
        .unwrap();
    cycle!(tester, "signing");
    let signers_for_transaction = &fixture.signers_fixture()[2..=6];
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            signers_for_transaction,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoTransactions");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
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
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester.increase_epoch().await.unwrap();
    cycle!(tester, "idle");

    cycle!(tester, "ready");

    assert_metrics_eq!(
        tester.metrics_verifier,
        ExpectedMetrics::new()
            .certificate_total(4)
            .artifact_cardano_database_total(1)
            .artifact_mithril_stake_distribution_total(1)
            .artifact_cardano_transaction_total(2)
    );
}
