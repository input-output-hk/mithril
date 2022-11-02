mod test_extensions;

use mithril_common::crypto_helper::tests_setup;
use mithril_common::entities::SignerWithStake;
use mithril_common::entities::{ProtocolMessagePartKey, ProtocolParameters};
use test_extensions::RuntimeTester;

#[tokio::test]
async fn create_certificate() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.95,
    };
    let mut tester = RuntimeTester::build(protocol_parameters.clone()).await;

    comment!("create signers & declare stake distribution");
    let signers = tests_setup::setup_signers(10, &protocol_parameters.clone().into());
    let signers_with_stake: Vec<SignerWithStake> = signers
        .clone()
        .into_iter()
        .map(|(signer_with_stake, _, _)| signer_with_stake)
        .collect();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;
    tester
        .deps
        .simulate_genesis(
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
    cycle!(tester, "signing");

    comment!("change the immutable number to alter the beacon");
    tester.increase_immutable_number().await.unwrap();
    cycle!(tester, "idle");
    cycle!(tester, "ready");
    cycle!(tester, "signing");

    comment!("signers send their single signature");
    tester.send_single_signatures(&signers).await.unwrap();

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
    cycle!(tester, "idle");
}
