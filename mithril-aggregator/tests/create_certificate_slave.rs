mod test_extensions;

use std::{collections::HashMap, ops::Range};

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoTransactionsSigningConfig, ChainPoint, Epoch, ProtocolParameters,
        SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber, StakeDistributionParty,
        TimePoint,
    },
    temp_dir,
    test_utils::{
        MithrilFixture, MithrilFixtureBuilder, StakeDistributionGenerationMethod, TempDir,
    },
};
use test_extensions::{utilities::get_test_dir, ExpectedCertificate, RuntimeTester};

/// Epoch fixtures helps using the fixtures in the tests
struct EpochFixtures<'a> {
    /// The fixture used for the registration of signers of the epoch
    registering: &'a MithrilFixture,
    /// The fixture used for the signing of the epoch
    current_signing: Option<&'a MithrilFixture>,
    /// The fixture used for the signing of the following epoch
    next_signing: Option<&'a MithrilFixture>,
}

/// Epoch fixtures map builders
struct EpochFixturesMapBuilder;

impl EpochFixturesMapBuilder {
    fn build_fixtures_sequence(
        epochs_range: Range<usize>,
        protocol_parameters: ProtocolParameters,
    ) -> HashMap<Epoch, MithrilFixture> {
        epochs_range
            .map(|epoch| {
                (
                    Epoch(epoch as u64),
                    MithrilFixtureBuilder::default()
                        .with_signers(epoch + 1)
                        .with_protocol_parameters(protocol_parameters.clone())
                        .with_stake_distribution(
                            StakeDistributionGenerationMethod::RandomDistribution {
                                seed: [epoch as u8; 32],
                            },
                        )
                        .build(),
                )
            })
            .collect::<HashMap<_, _>>()
    }

    fn build_epoch_fixtures_map(
        fixtures: &HashMap<Epoch, MithrilFixture>,
    ) -> HashMap<Epoch, EpochFixtures<'_>> {
        fixtures
            .iter()
            .map(|(Epoch(index), _fixture)| {
                (
                    Epoch(*index),
                    EpochFixtures {
                        registering: &fixtures[&Epoch(*index)],
                        next_signing: if *index >= 1 {
                            fixtures.get(&Epoch(*index - 1))
                        } else {
                            None
                        },
                        current_signing: if *index >= 2 {
                            fixtures.get(&Epoch(*index - 2))
                        } else {
                            None
                        },
                    },
                )
            })
            .collect::<HashMap<_, _>>()
    }
}

#[tokio::test]
async fn create_certificate_slave() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let fixtures =
        EpochFixturesMapBuilder::build_fixtures_sequence(1..10, protocol_parameters.clone());
    let epoch_fixtures_map = EpochFixturesMapBuilder::build_epoch_fixtures_map(&fixtures);
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
    - the master aggregator registers the first signers
    - the master aggregator can't transition from 'Idle' to 'Ready'
    - the slave aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(1)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Master: start the runtime state machine");
    cycle_err!(master_tester, "idle");
    cycle_err!(master_tester, "idle");

    comment!("Slave: start the runtime state machine");
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

    comment!("Master: register signers");
    master_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle_err!(master_tester, "idle");

    comment!(
        "Epoch 2:
    - the master aggregator creates its genesis certificate
    - the master aggregator can't transition from 'Idle' to 'Ready'
    - the slave aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(2)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle_err!(master_tester, "idle");
    cycle_err!(master_tester, "idle");

    comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

    comment!("Master: register signers");
    master_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle_err!(master_tester, "idle");

    comment!("Master: bootstrap the genesis certificate");
    master_tester
        .register_genesis_certificate(epoch_fixture.next_signing.unwrap())
        .await
        .unwrap();

    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new_genesis(
            Epoch(2),
            epoch_fixture.next_signing.unwrap().compute_and_encode_avk()
        )
    );

    comment!(
        "Epoch 3:
    - the master aggregator produces a new certificate
    - the slave aggregator synchronizes signers from the master aggregator
    - the slave aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(3)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Master: change the epoch");
    master_tester.increase_epoch().await.unwrap();
    cycle!(master_tester, "ready");
    cycle!(master_tester, "signing");

    comment!("Slave: change the epoch after master");
    slave_tester.increase_epoch().await.unwrap();
    cycle_err!(slave_tester, "idle");
    cycle_err!(slave_tester, "idle");

    comment!("Master: register signers");
    master_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();
    cycle!(master_tester, "ready");

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "signing");

    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new(
            Epoch(3),
            StakeDistributionParty::from_signers(
                epoch_fixture.current_signing.unwrap().signers_with_stake()
            )
            .as_slice(),
            epoch_fixture
                .current_signing
                .unwrap()
                .compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(3)),
            ExpectedCertificate::genesis_identifier(Epoch(2)),
        )
    );
    cycle_err!(master_tester, "signing");

    comment!(
        "Epoch 4:
    - the master aggregator produces a new certificate
    - the slave aggregator synchronizes signers from the master aggregator
    - the slave aggregator bootstraps its genesis certificate
    - the slave aggregator can't transition from 'Idle' to 'Ready'"
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(4)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

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
        .register_genesis_certificate(epoch_fixture.next_signing.unwrap())
        .await
        .unwrap();

    comment!("Master: register signers");
    master_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");

    assert_last_certificate_eq!(
        master_tester,
        ExpectedCertificate::new(
            Epoch(4),
            StakeDistributionParty::from_signers(
                epoch_fixture.current_signing.unwrap().signers_with_stake()
            )
            .as_slice(),
            epoch_fixture
                .current_signing
                .unwrap()
                .compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(4)),
            ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(3))),
        )
    );
    cycle!(master_tester, "signing");

    comment!(
        "Epoch 5:
    - the master aggregator produces a new certificate
    - the slave aggregator produces a new certificate
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    ");
    let epoch_fixture = &epoch_fixtures_map[&Epoch(5)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
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
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(5),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(5)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(4))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(5),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(5)),
        ExpectedCertificate::genesis_identifier(Epoch(4)),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = epoch_fixture
        .current_signing
        .unwrap()
        .compute_and_encode_avk()
        .clone();
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());

    comment!(
        "Epoch 6:
    - the master aggregator produces a new certificate
    - the slave aggregator produces a new certificate
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    ");
    let epoch_fixture = &epoch_fixtures_map[&Epoch(6)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

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
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(6),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(6)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(5))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(6),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(6)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(5))),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = epoch_fixture
        .current_signing
        .unwrap()
        .compute_and_encode_avk()
        .clone();
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());

    comment!(
        "Epoch 7:
    - the master aggregator produces a new certificate
    - the slave aggregator produces a new certificate
    - the slave aggregator new certificate uses the same avk as the master aggregator's new certificate
    ");
    let epoch_fixture = &epoch_fixtures_map[&Epoch(7)];

    comment!("Master: update stake distribution source");
    master_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Slave: update stake distribution source");
    slave_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

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
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(master_tester, "signing");

    comment!("Master: signers send their single signature");
    master_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Slave: signers send their single signature");
    cycle!(slave_tester, "signing");
    slave_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Master: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(master_tester, "ready");
    let master_expected_certificate = ExpectedCertificate::new(
        Epoch(7),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(7)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(6))),
    );
    assert_last_certificate_eq!(master_tester, master_expected_certificate);

    comment!("Slave: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(slave_tester, "ready");
    let slave_expected_certificate = ExpectedCertificate::new(
        Epoch(7),
        StakeDistributionParty::from_signers(
            epoch_fixture.current_signing.unwrap().signers_with_stake(),
        )
        .as_slice(),
        epoch_fixture
            .current_signing
            .unwrap()
            .compute_and_encode_avk(),
        SignedEntityType::MithrilStakeDistribution(Epoch(7)),
        ExpectedCertificate::identifier(&SignedEntityType::MithrilStakeDistribution(Epoch(6))),
    );
    assert_last_certificate_eq!(slave_tester, slave_expected_certificate);
    let expected_avk = epoch_fixture
        .current_signing
        .unwrap()
        .compute_and_encode_avk()
        .clone();
    assert_eq!(expected_avk, master_expected_certificate.avk());
    assert_eq!(expected_avk, slave_expected_certificate.avk());
}
