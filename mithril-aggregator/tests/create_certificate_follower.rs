mod test_extensions;

use std::{collections::HashMap, ops::Range};

use mithril_aggregator::ServeCommandConfiguration;
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
                        .with_signers(epoch)
                        .with_protocol_parameters(protocol_parameters.clone())
                        .with_stake_distribution(
                            StakeDistributionGenerationMethod::RandomDistribution {
                                seed: [epoch as u8; 32],
                                min_stake: 10,
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
                        next_signing: index
                            .checked_sub(1)
                            .map(Epoch)
                            .and_then(|e| fixtures.get(&e)),
                        current_signing: index
                            .checked_sub(2)
                            .map(Epoch)
                            .and_then(|e| fixtures.get(&e)),
                    },
                )
            })
            .collect::<HashMap<_, _>>()
    }
}

#[tokio::test]
async fn create_certificate_follower() {
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
    let leader_configuration = ServeCommandConfiguration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("create_certificate_leader"),
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(0),
            step: BlockNumber(30),
        },
        signed_entity_types: Some(SignedEntityTypeDiscriminants::CardanoDatabase.to_string()),
        ..ServeCommandConfiguration::new_sample(temp_dir!())
    };
    let mut leader_tester =
        RuntimeTester::build(start_time_point.clone(), leader_configuration.clone()).await;
    let leader_aggregator_http_server = leader_tester.expose_epoch_settings().await.unwrap();

    let follower_configuration = ServeCommandConfiguration {
        data_stores_directory: get_test_dir("create_certificate_follower"),
        snapshot_directory: TempDir::create(
            "aggregator-integration",
            "create_certificate_follower",
        ),
        leader_aggregator_endpoint: Some(leader_aggregator_http_server.url()),
        ..leader_configuration
    };
    let mut follower_tester = RuntimeTester::build(start_time_point, follower_configuration).await;

    comment!(
        "Epoch 1:
    - the leader aggregator registers the first signers
    - the leader aggregator can't transition from 'Idle' to 'Ready'
    - the follower aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(1)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Leader: start the runtime state machine");
    cycle_err!(leader_tester, "idle");

    comment!("Follower: start the runtime state machine");
    cycle_err!(follower_tester, "idle");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle_err!(leader_tester, "idle");

    comment!(
        "Epoch 2:
    - the leader aggregator creates its genesis certificate
    - the leader aggregator can't transition from 'Idle' to 'Ready'
    - the follower aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(2)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle_err!(leader_tester, "idle");

    comment!("Follower: change the epoch after leader");
    follower_tester.increase_epoch().await.unwrap();
    cycle_err!(follower_tester, "idle");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle_err!(leader_tester, "idle");

    comment!("Leader: bootstrap the genesis certificate");
    leader_tester
        .register_genesis_certificate(epoch_fixture.next_signing.unwrap())
        .await
        .unwrap();

    assert_last_certificate_eq!(
        leader_tester,
        ExpectedCertificate::new_genesis(
            Epoch(2),
            epoch_fixture.next_signing.unwrap().compute_and_encode_avk()
        )
    );

    comment!(
        "Epoch 3:
    - the leader aggregator produces a new certificate
    - the follower aggregator synchronizes signers from the leader aggregator
    - the follower aggregator can't transition from 'Idle' to 'Ready'
    "
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(3)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle!(leader_tester, "ready");
    cycle!(leader_tester, "signing");

    comment!("Follower: change the epoch after leader");
    follower_tester.increase_epoch().await.unwrap();
    cycle_err!(follower_tester, "idle");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();

    comment!("Leader: signers send their single signature");
    leader_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();
    cycle!(leader_tester, "ready");

    comment!("Leader: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(leader_tester, "signing");

    assert_last_certificate_eq!(
        leader_tester,
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
    cycle_err!(leader_tester, "signing");

    comment!(
        "Epoch 4:
    - the leader aggregator produces a new certificate
    - the follower aggregator synchronizes signers from the leader aggregator
    - the follower aggregator bootstraps its genesis certificate
    - the follower aggregator can't transition from 'Idle' to 'Ready'"
    );
    let epoch_fixture = &epoch_fixtures_map[&Epoch(4)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle!(leader_tester, "idle");
    cycle!(leader_tester, "ready");

    comment!("Follower: change the epoch after leader");
    follower_tester.increase_epoch().await.unwrap();
    cycle_err!(follower_tester, "idle");

    comment!("Follower: bootstrap the genesis certificate");
    follower_tester
        .register_genesis_certificate(epoch_fixture.next_signing.unwrap())
        .await
        .unwrap();

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(leader_tester, "signing");

    comment!("Leader: signers send their single signature");
    leader_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Leader: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(leader_tester, "ready");

    assert_last_certificate_eq!(
        leader_tester,
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
    cycle!(leader_tester, "signing");

    comment!(
        "Epoch 5:
    - the leader aggregator produces a new certificate
    - the follower aggregator produces a new certificate
    - the follower aggregator new certificate uses the same avk as the leader aggregator's new certificate
    ");
    let epoch_fixture = &epoch_fixtures_map[&Epoch(5)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: change the epoch before leader");
    follower_tester.increase_epoch().await.unwrap();
    cycle!(follower_tester, "idle");

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle!(leader_tester, "idle");
    cycle!(leader_tester, "ready");

    comment!("Follower: change the epoch after leader");
    cycle!(follower_tester, "ready");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(leader_tester, "signing");

    comment!("Leader: signers send their single signature");
    leader_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Follower: signers send their single signature");
    cycle!(follower_tester, "signing");
    follower_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Leader: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(leader_tester, "ready");
    let leader_expected_certificate = ExpectedCertificate::new(
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
    assert_last_certificate_eq!(leader_tester, leader_expected_certificate);

    comment!("Follower: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(follower_tester, "ready");
    let follower_expected_certificate = ExpectedCertificate::new(
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
    assert_last_certificate_eq!(follower_tester, follower_expected_certificate);
    let expected_avk = epoch_fixture
        .current_signing
        .unwrap()
        .compute_and_encode_avk()
        .clone();
    assert_eq!(expected_avk, leader_expected_certificate.avk());
    assert_eq!(expected_avk, follower_expected_certificate.avk());

    comment!(
        "Epoch 6:
    - the leader aggregator produces a new certificate
    - the follower aggregator produces a new certificate
    - the follower aggregator new certificate uses the same avk as the leader aggregator's new certificate
    ");
    let epoch_fixture = &epoch_fixtures_map[&Epoch(6)];

    comment!("Leader: update stake distribution source");
    leader_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Follower: update stake distribution source");
    follower_tester
        .update_stake_distribution(epoch_fixture.registering.stake_distribution())
        .await
        .unwrap();

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle!(leader_tester, "idle");
    cycle!(leader_tester, "ready");

    comment!("Follower: change the epoch after leader");
    follower_tester.increase_epoch().await.unwrap();
    cycle!(follower_tester, "idle");
    cycle!(follower_tester, "ready");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&epoch_fixture.registering.signers_fixture())
        .await
        .unwrap();
    cycle!(leader_tester, "signing");

    comment!("Leader: signers send their single signature");
    leader_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Follower: signers send their single signature");
    cycle!(follower_tester, "signing");
    follower_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &epoch_fixture.current_signing.unwrap().signers_fixture(),
        )
        .await
        .unwrap();

    comment!("Leader: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(leader_tester, "ready");
    let leader_expected_certificate = ExpectedCertificate::new(
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
    assert_last_certificate_eq!(leader_tester, leader_expected_certificate);

    comment!("Follower: state machine should issue a certificate for the MithrilStakeDistribution");
    cycle!(follower_tester, "ready");
    let follower_expected_certificate = ExpectedCertificate::new(
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
    assert_last_certificate_eq!(follower_tester, follower_expected_certificate);
    let expected_avk = epoch_fixture
        .current_signing
        .unwrap()
        .compute_and_encode_avk()
        .clone();
    assert_eq!(expected_avk, leader_expected_certificate.avk());
    assert_eq!(expected_avk, follower_expected_certificate.avk());
}
