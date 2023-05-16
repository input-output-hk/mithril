mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{
        Beacon, Epoch, ProtocolParameters, SignedEntityType, SignedEntityTypeDiscriminants,
    },
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::{utilities::get_test_dir, ExpectedCertificate, RuntimeTester};

#[tokio::test]
async fn create_certificate() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("create_certificate").join("aggregator.sqlite3"),
        ..Configuration::new_sample()
    };
    let mut tester =
        RuntimeTester::build(Beacon::new("devnet".to_string(), 1, 1), configuration).await;

    comment!("create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();

    tester.init_state_from_fixture(&fixture).await.unwrap();

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&fixture).await.unwrap();

    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(
            Beacon::new("devnet".to_string(), 1, 1),
            fixture.compute_and_encode_avk()
        )
    );

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("start the runtime state machine");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!("register signers");
    tester
        .register_signers(&fixture.signers_fixture())
        .await
        .unwrap();
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
            Beacon::new("devnet".to_string(), 1, 2),
            &fixture.signers_with_stake(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!("The state machine should get back to signing to sign CardanoImmutableFilesFull");
    // todo!: remove this immutable increase:
    // right now because we only have one state machine for all signed entity type we need it else
    // the state machine will stay in the idle state since its beacon didn't change.
    // With one state machine per signed entity type this problem will disappear.
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    let signers_for_immutables = &fixture.signers_fixture()[0..=6];
    tester
        .send_single_signatures(
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            signers_for_immutables,
        )
        .await
        .unwrap();

    comment!("The state machine should issue a certificate for the CardanoImmutableFilesFull");
    cycle!(tester, "ready");
    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new(
            Beacon::new("devnet".to_string(), 1, 3),
            &signers_for_immutables
                .iter()
                .map(|s| s.into())
                .collect::<Vec<_>>(),
            fixture.compute_and_encode_avk(),
            SignedEntityType::CardanoImmutableFilesFull(Beacon::new("devnet".to_string(), 1, 3)),
            ExpectedCertificate::genesis_identifier(&Beacon::new("devnet".to_string(), 1, 1)),
        )
    );

    comment!("Change the epoch while signing");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "signing");
    tester.increase_epoch().await.unwrap();
    cycle!(tester, "idle");

    cycle!(tester, "ready");
}
