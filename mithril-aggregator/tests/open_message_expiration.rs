mod test_extensions;

use std::time::Duration;

use mithril_aggregator::ServeCommandConfiguration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoDbBeacon, ChainPoint, Epoch, ProtocolParameters, SignedEntityType,
        SignedEntityTypeDiscriminants, SlotNumber, TimePoint,
    },
    temp_dir,
    test::builder::MithrilFixtureBuilder,
};
use test_extensions::{
    ExpectedCertificate, ExpectedMetrics, RuntimeTester, utilities::get_test_dir,
};

#[tokio::test]
async fn open_message_expiration() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let configuration = ServeCommandConfiguration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("open_message_expiration"),
        signed_entity_types: Some(SignedEntityTypeDiscriminants::CardanoDatabase.to_string()),
        ..ServeCommandConfiguration::new_sample(temp_dir!())
    };
    let mut tester = RuntimeTester::build(
        TimePoint::new(
            1,
            1,
            ChainPoint::new(SlotNumber(10), BlockNumber(1), "block_hash-1"),
        ),
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

    comment!(
        "Schedule the open message for MithrilStakeDistribution to expire, and wait until it does"
    );
    let open_message_timeout = Duration::from_millis(100);
    tester
        .activate_open_message_expiration(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            open_message_timeout,
        )
        .await
        .unwrap();
    tokio::time::sleep(2 * open_message_timeout).await;

    comment!("signers send their single signature");
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();

    comment!("The state machine should not issue a certificate for the MithrilStakeDistribution");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(Epoch(1), fixture.compute_and_encode_avk())
    );

    comment!("Increase the immutable file number");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");

    comment!("The state machine should get back to signing to sign CardanoDatabase");
    let signers_for_immutables = &fixture.signers_fixture()[0..=6];
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoDatabase,
            signers_for_immutables,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoDatabase");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Epoch(1),
            &signers_for_immutables
                .iter()
                .map(|s| s.signer_with_stake.clone().into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(1, 3)),
            ExpectedCertificate::genesis_identifier(Epoch(1)),
        )
    );

    assert_metrics_eq!(
        tester.metrics_verifier,
        ExpectedMetrics::new()
            .certificate_total(1)
            .artifact_cardano_database_total(1)
    );
}
