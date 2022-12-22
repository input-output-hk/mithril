mod test_extensions;

use mithril_common::{
    entities::{ProtocolParameters, SignerWithStake},
    test_utils::MithrilFixtureBuilder,
};
use test_extensions::RuntimeTester;

#[tokio::test]
async fn simple_scenario() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.65,
    };
    let mut tester = RuntimeTester::build(protocol_parameters.clone()).await;

    comment!("Create signers & declare stake distribution");
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(5)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    let signers = fixture.signers_fixture();
    let signers_with_stake: Vec<SignerWithStake> = fixture.signers_with_stake();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;
    tester
        .deps
        .prepare_for_genesis(
            signers_with_stake.clone(),
            signers_with_stake.clone(),
            &protocol_parameters,
        )
        .await;

    cycle!(tester, "idle");

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&signers).await.unwrap();

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    cycle!(tester, "ready");
    cycle!(tester, "signing");
}
