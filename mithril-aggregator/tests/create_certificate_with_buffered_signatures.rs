mod test_extensions;

use mithril_aggregator::ServeCommandConfiguration;
use mithril_common::{
    entities::{
        BlockNumber, CardanoTransactionsSigningConfig, ChainPoint, Epoch, ProtocolParameters,
        SignedEntityType, SignedEntityTypeDiscriminants, SlotNumber, StakeDistributionParty,
        TimePoint,
    },
    temp_dir,
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::{
    utilities::get_test_dir, ExpectedCertificate, ExpectedMetrics, RuntimeTester,
};

#[tokio::test]
async fn create_certificate_with_buffered_signatures() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 150,
        phi_f: 0.95,
    };
    let configuration = ServeCommandConfiguration {
        protocol_parameters: protocol_parameters.clone(),
        signed_entity_types: Some(SignedEntityTypeDiscriminants::CardanoTransactions.to_string()),
        data_stores_directory: get_test_dir("create_certificate_with_buffered_signatures"),
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

    comment!("signers send their single signature before the state machine is signing");
    tester
        .send_authenticated_single_signatures(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            &fixture.signers_fixture(),
        )
        .await
        .unwrap();
    cycle!(tester, "signing");

    comment!("register signers");
    tester.register_signers(&fixture.signers_fixture()).await.unwrap();

    comment!("Using buffered signatures, the state machine should issue a certificate for the MithrilStakeDistribution");
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

    tester.increase_epoch().await.unwrap();
    cycle!(tester, "idle");

    cycle!(tester, "ready");

    assert_metrics_eq!(
        tester.metrics_verifier,
        ExpectedMetrics::new()
            .certificate_total(1)
            .artifact_mithril_stake_distribution_total(1)
    );
}
