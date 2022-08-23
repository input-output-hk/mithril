mod test_extensions;

use mithril_common::{
    crypto_helper::tests_setup,
    entities::{ProtocolParameters, SignerWithStake},
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
    let signers = tests_setup::setup_signers(5, &protocol_parameters.clone().into());
    let signers_with_stake: Vec<SignerWithStake> =
        signers.clone().into_iter().map(|s| s.into()).collect();
    tester
        .chain_observer
        .set_signers(signers_with_stake.clone())
        .await;
    tester
        .deps
        .simulate_genesis(
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
