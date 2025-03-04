mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoTransactionsSigningConfig, ChainPoint, Epoch, ProtocolParameters,
        SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber, StakeDistributionParty,
        TimePoint,
    },
    test_utils::{MithrilFixtureBuilder, TempDir},
};
use test_extensions::{utilities::get_test_dir, ExpectedCertificate, RuntimeTester};

#[tokio::test]
async fn create_certificate_slave() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let start_time_point = TimePoint {
        epoch: Epoch(1),
        immutable_file_number: 1,
        chain_point: ChainPoint {
            slot_number: SlotNumber(10),
            block_number: BlockNumber(100),
            block_hash: "block_hash-100".to_string(),
        },
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
    let mut master_tester =
        RuntimeTester::build(start_time_point.clone(), master_configuration.clone()).await;
    let master_aggregator_http_server = master_tester.expose_epoch_settings().await.unwrap();

    let slave_configuration = Configuration {
        data_stores_directory: get_test_dir("create_certificate_slave"),
        snapshot_directory: TempDir::create("aggregator-integration", "create_certificate_slave"),
        master_aggregator_endpoint: Some(master_aggregator_http_server.url()),
        ..master_configuration
    };
    let mut slave_tester = RuntimeTester::build(start_time_point, slave_configuration).await;

    comment!("Master: create signers & declare stake distribution");
    master_tester
        .init_state_from_fixture(&fixture)
        .await
        .unwrap();

    comment!("Slave: create signers & declare stake distribution");
    slave_tester
        .chain_observer
        .set_signers(fixture.signers_with_stake())
        .await;

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

    comment!("Slave: start the runtime state machine");
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

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

    // New epoch
    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

    comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

    comment!("Slave: bootstrap the genesis certificate");
    slave_tester
        .register_genesis_certificate(&fixture)
        .await
        .unwrap();

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

    // New epoch
    /* comment!("Slave: change the epoch before master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "idle"); */

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    //cycle!(master_tester, "ready");

    comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "ready");

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
            Epoch(3),
            StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(3)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(2))),
        )
    );

    // New epoch
    /* comment!("Slave: change the epoch before master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "idle"); */

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

    /* comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "ready"); */

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
            Epoch(4),
            StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(4)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(3))),
        )
    );
}
