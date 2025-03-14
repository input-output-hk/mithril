mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoTransactionsSigningConfig, ChainPoint, Epoch, ProtocolParameters,
        SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber, StakeDistribution,
        StakeDistributionParty, TimePoint,
    },
    temp_dir,
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
    let current_fixture = MithrilFixtureBuilder::default()
        .with_signers(2)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let current_avk = current_fixture.compute_and_encode_avk();
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
        data_stores_directory: get_test_dir("create_certificate_master"),
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(0),
            step: BlockNumber(30),
        },
        ..Configuration::new_sample(temp_dir!())
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

    comment!(
        "Epoch 1:
    - the master aggregator bootstraps its genesis certificate
    - the slave aggregator synchronizes signers from the master aggregator
    - the slave aggregator stays in Idle state with an error as it doesn't have a genesis certificate yet
    ");

    comment!("Master: create signers & declare stake distribution");
    master_tester
        .init_state_from_fixture(&current_fixture)
        .await
        .unwrap();

    comment!("Slave: create signers & declare stake distribution");
    slave_tester
        .chain_observer
        .set_signers(current_fixture.signers_with_stake())
        .await;

    comment!("Master: bootstrap the genesis certificate");
    master_tester
        .register_genesis_certificate(&current_fixture)
        .await
        .unwrap();

    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new_genesis(Epoch(1), current_fixture.compute_and_encode_avk())
    );

    comment!("Master: start the runtime state machine");
    cycle!(master_tester, "ready");
    cycle!(master_tester, "signing");

    comment!("Slave: start the runtime state machine");
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

    comment!("Master: register signers");
    master_tester
        .register_signers(&current_fixture.signers_fixture())
        .await
        .unwrap();
    cycle_err!(master_tester, "signing");

    comment!(
        "Epoch 2:
    - the master aggregator produces a new certificate
    - the slave aggregator synchronizes signers from the master aggregator
    - the slave aggregator bootstraps its genesis certificate"
    );

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
        .register_genesis_certificate(&current_fixture)
        .await
        .unwrap();

    comment!("Master: register signers");
    master_tester
        .register_signers(&current_fixture.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &current_fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new(
            Epoch(2),
            StakeDistributionParty::from_signers(current_fixture.signers_with_stake()).as_slice(),
            current_fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(2)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );
    cycle!(master_tester, "signing");

    comment!(
        "Epoch 3:
    - the master aggregator produces a new certificate
    - the master aggregator stake distribution is updated
    - the slave aggregator can't transition from 'Idle' to 'Ready' when the master aggregator has not transitioned to a new epoch
    - the slave aggregator can transition from 'Idle' to 'Ready' when the master aggregator has transitioned to a new epoch
    - the slave aggregator produces a new certificate
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    - the slave aggregator stake distribution is updated
    ");

    comment!("Master: update stake distribution");
    let following_fixture = {
        let updated_stake_distribution = StakeDistribution::from_iter(
            current_fixture
                .signers_with_stake()
                .into_iter()
                .map(|s| (s.party_id, s.stake + 1000)),
        );

        master_tester
            .update_stake_distribution(updated_stake_distribution)
            .await
            .unwrap()
    };
    let following_avk = following_fixture.compute_and_encode_avk();

    comment!("Slave: update stake distribution");
    slave_tester
        .update_stake_distribution(following_fixture.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: change the epoch before master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "idle");

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

    comment!("Slave: change the epoch after master");
    cycle!(slave_tester, "ready");

    comment!("Master: register signers");
    master_tester
        .register_signers(&current_fixture.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &current_fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &current_fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(3),
        StakeDistributionParty::from_signers(current_fixture.signers_with_stake()).as_slice(),
        current_fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(2))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(3),
        StakeDistributionParty::from_signers(current_fixture.signers_with_stake()).as_slice(),
        current_fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
        ExpectedCertificate::genesis_identifier(Epoch(2)),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = current_avk.clone();
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());

    comment!(
        "Epoch 4:
    - the master aggregator produces a new certificate
    - the slave aggregator produces a new certificate
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    ");

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

    comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle!(slave_tester, "idle");
    cycle!(slave_tester, "ready");

    comment!("Master: register signers");
    master_tester
        .register_signers(&current_fixture.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &current_fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &current_fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(4),
        StakeDistributionParty::from_signers(current_fixture.signers_with_stake()).as_slice(),
        current_fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(4)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(3))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(4),
        StakeDistributionParty::from_signers(current_fixture.signers_with_stake()).as_slice(),
        current_fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(4)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(3))),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = current_avk;
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());

    comment!(
        "Epoch 5:
    - the master aggregator produces a new certificate with the new stake distribution from epoch 3
    - the slave aggregator produces a new certificate with the new stake distribution from epoch 3
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    ");
    let fixture = following_fixture;

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "idle");
    cycle!(master_tester, "ready");

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

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(5),
        StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
        fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(5)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(4))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(5),
        StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
        fixture.compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(5)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(4))),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = following_avk;
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());
}
