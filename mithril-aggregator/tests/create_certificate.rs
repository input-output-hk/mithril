mod test_extensions;

use std::collections::BTreeSet;

use mithril_aggregator::Configuration;
use mithril_common::{
    entities::{ProtocolMessagePartKey, ProtocolParameters},
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::{utilities::get_test_dir, RuntimeTester};

#[tokio::test]
async fn create_certificate() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.95,
    };
    let configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("create_certificate").join("aggregator.sqlite3"),
        ..Configuration::new_sample()
    };
    let mut tester = RuntimeTester::build(configuration).await;

    comment!("create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(10)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let signers = fixture.signers_fixture();
    let signers_with_stake = fixture.signers_with_stake();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;
    tester
        .deps_builder
        .build_dependency_container()
        .await
        .unwrap()
        .prepare_for_genesis(
            signers_with_stake.clone(),
            signers_with_stake,
            &protocol_parameters,
        )
        .await;

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&signers).await.unwrap();

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("start the runtime state machine");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!("register signers");
    tester.register_signers(&signers).await.unwrap();
    cycle_err!(tester, "signing");

    comment!("change the immutable number to alter the beacon");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "idle");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!("signers send their single signature");
    let signed_entity_type = tester.retrieve_signed_entity_type().await;
    let signers_who_sign = &signers[0..=6];
    tester
        .send_single_signatures(&signed_entity_type, signers_who_sign)
        .await
        .unwrap();

    comment!("The state machine should issue a multisignature");
    cycle!(tester, "idle");
    let (last_certificates, snapshots) =
        tester.get_last_certificates_and_snapshots().await.unwrap();

    assert_eq!((2, 1), (last_certificates.len(), snapshots.len()));
    assert_eq!(
        (
            &last_certificates[0].hash,
            last_certificates[0]
                .protocol_message
                .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
                .unwrap()
        ),
        (&snapshots[0].certificate_hash, &snapshots[0].digest)
    );
    assert_eq!(
        BTreeSet::from_iter(
            last_certificates[0]
                .metadata
                .signers
                .iter()
                .map(|s| s.party_id.to_owned())
        ),
        BTreeSet::from_iter(
            signers_who_sign
                .iter()
                .map(|s| s.signer_with_stake.party_id.to_owned())
        )
    );

    cycle!(tester, "idle");
}
