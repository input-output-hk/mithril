mod test_extensions;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{CardanoDbBeacon, ChainPoint, ProtocolParameters, TimePoint},
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::{utilities::get_test_dir, ExpectedCertificate, RuntimeTester};

#[tokio::test]
async fn genesis_to_signing() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.65,
    };
    let configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("genesis_to_signing"),
        ..Configuration::new_sample()
    };
    let mut tester = RuntimeTester::build(
        TimePoint::new(1, 1, ChainPoint::new(10, 1, "block_hash-1")),
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

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&fixture).await.unwrap();

    assert_last_certificate_eq!(
        tester,
        ExpectedCertificate::new_genesis(
            CardanoDbBeacon::new("devnet".to_string(), 1, 1),
            fixture.compute_and_encode_avk()
        )
    );

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    cycle!(tester, "ready");
    cycle!(tester, "signing");
}
