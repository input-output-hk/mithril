mod test_extensions;

use mithril_aggregator::ServeCommandConfiguration;
use mithril_common::{
    entities::{BlockNumber, ChainPoint, Epoch, ProtocolParameters, SlotNumber, TimePoint},
    temp_dir,
    test::builder::MithrilFixtureBuilder,
};
use test_extensions::{ExpectedCertificate, RuntimeTester, utilities::get_test_dir};

#[tokio::test]
async fn genesis_to_signing() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.65,
    };
    let configuration = ServeCommandConfiguration {
        protocol_parameters: Some(protocol_parameters.clone()),
        data_stores_directory: get_test_dir("genesis_to_signing"),
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

    comment!("Create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(5)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    tester.init_state_from_fixture(&fixture).await.unwrap();

    cycle_err!(tester, "idle");

    comment!("Bootstrap the genesis certificate");
    tester.register_genesis_certificate(&fixture).await.unwrap();

    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(Epoch(1), fixture.compute_and_encode_concatenation_aggregate_verification_key())
    );

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    cycle!(tester, "ready");
    cycle!(tester, "signing");
}
