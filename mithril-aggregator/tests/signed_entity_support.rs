mod test_extensions;

use mithril_aggregator::ServeCommandConfiguration;
use mithril_common::{
    current_function,
    entities::{
        BlockNumber, ChainPoint, Epoch, ProtocolParameters, SignedEntityType,
        SignedEntityTypeDiscriminants, SlotNumber, StakeDistributionParty, TimePoint,
    },
    temp_dir,
    test::builder::MithrilFixtureBuilder,
};
use test_extensions::{ExpectedCertificate, RuntimeTester, utilities::get_test_dir};

#[tokio::test]
async fn follower_can_cycle_to_ready_to_signing_with_unknown_and_discontinued_signed_entity() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let test_dir = get_test_dir(current_function!());
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
        protocol_parameters: Some(protocol_parameters.clone()),
        data_stores_directory: test_dir.join("leader"),
        signed_entity_types: None,
        ..ServeCommandConfiguration::new_sample(temp_dir!())
    };
    let mut leader_tester =
        RuntimeTester::build(start_time_point.clone(), leader_configuration.clone()).await;
    let leader_aggregator_http_server =
        leader_tester.spawn_leader_aggregator_http_server().await.unwrap();

    let follower_configuration = ServeCommandConfiguration {
        data_stores_directory: test_dir.join("follower"),
        snapshot_directory: test_dir.join("follower/snapshot"),
        leader_aggregator_endpoint: Some(leader_aggregator_http_server.url().to_string()),
        // Follower must retrieve parameters from the network configuration (today through the leader)
        // so this parameters should not be read
        protocol_parameters: None,
        ..leader_configuration
    };
    let mut follower_tester = RuntimeTester::build(start_time_point, follower_configuration).await;

    comment!("create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let signers = fixture.signers_fixture();

    leader_tester.init_state_from_fixture(&fixture).await.unwrap();
    follower_tester
        .update_stake_distribution(fixture.stake_distribution())
        .await
        .unwrap();

    comment!(
        "Epoch 1:
    - startup both the leader and follower
    - the leader aggregator can't transition to 'Ready' yet because genesis certificate is not registered
    - the follower can't transition to 'Ready' yet"
    );

    comment!("Leader: start the runtime state machine");
    cycle_err!(leader_tester, "idle");

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&fixture.signers_fixture())
        .await
        .unwrap();

    comment!("Follower: start the runtime state machine");
    cycle_err!(follower_tester, "idle");

    comment!(
        "Epoch 2:
    - bootstrap the genesis certificate on the leader aggregator
    - the leader aggregator can transition to 'Signing'
    - the follower startup but can't go to 'Ready' yet"
    );

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();

    comment!("Leader: bootstrap the genesis certificate");
    leader_tester.register_genesis_certificate(&fixture).await.unwrap();
    cycle!(leader_tester, "ready");

    assert_last_certificate_eq!(
        leader_tester,
        ExpectedCertificate::new_genesis(
            Epoch(2),
            fixture.compute_and_encode_concatenation_aggregate_verification_key()
        )
    );

    comment!("Leader: register signers");
    leader_tester
        .register_signers(&fixture.signers_fixture())
        .await
        .unwrap();

    comment!("Leader: can go to signing");
    cycle!(leader_tester, "signing");

    comment!("Follower: change the epoch after leader created a certificate");
    follower_tester.increase_epoch().await.unwrap();
    cycle_err!(follower_tester, "idle");

    comment!(
        "Epoch 3:
    - the follower aggregator sign a MithrilStakeDistribution"
    );

    comment!("Leader: change the epoch");
    leader_tester.increase_epoch().await.unwrap();
    cycle!(leader_tester, "idle");
    cycle!(leader_tester, "ready");

    comment!("Follower: change the epoch after leader created a certificate");
    follower_tester.increase_epoch().await.unwrap();

    cycle_err!(follower_tester, "idle");
    cycle!(follower_tester, "ready");
    cycle!(follower_tester, "signing");

    comment!("Follower: signers send their single signature");
    follower_tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &signers,
        )
        .await
        .unwrap();
    cycle!(follower_tester, "ready");

    let expected_certificate = ExpectedCertificate::new(
        Epoch(3),
        StakeDistributionParty::from_signers(fixture.signers_with_stake()).as_slice(),
        fixture.compute_and_encode_concatenation_aggregate_verification_key(),
        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
        ExpectedCertificate::genesis_identifier(Epoch(2)),
    );
    assert_last_certificate_eq!(follower_tester, expected_certificate);
}
