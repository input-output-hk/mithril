mod init;
use init::initialize_dependencies;
use mithril_aggregator::{
    AggregatorRunner, AggregatorRuntime, BeaconProviderImpl, DumbImmutableFileObserver,
};
use mithril_common::chain_observer::FakeObserver;
use mithril_common::crypto_helper::ProtocolSignerVerificationKey;
use mithril_common::crypto_helper::{key_decode_hex, ProtocolPartyId};
use mithril_common::digesters::DumbDigester;
use mithril_common::fake_data;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

#[tokio::test]
async fn create_certificate() {
    // initialization
    let (mut deps, config) = initialize_dependencies().await;
    let immutable_file_observer = Arc::new(RwLock::new(DumbImmutableFileObserver::default()));
    let chain_observer = Arc::new(RwLock::new(FakeObserver::new()));
    let beacon_provider = Arc::new(RwLock::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        mithril_common::CardanoNetwork::TestNet(42),
    )));
    let digester = Arc::new(DumbDigester::default());
    deps.with_immutable_file_observer(immutable_file_observer.clone())
        .with_beacon_provider(beacon_provider)
        .with_chain_observer(chain_observer)
        .with_digester(digester.clone());
    let deps = Arc::new(deps);
    let runner = Arc::new(AggregatorRunner::new(config.clone(), deps.clone()));
    let mut runtime =
        AggregatorRuntime::new(Duration::from_millis(config.interval), None, runner.clone())
            .await
            .unwrap();

    // start the runtime state machine
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());

    // register two signers
    let signers = fake_data::signers(2);

    {
        let mut multisigner = deps.multi_signer.as_ref().unwrap().write().await;

        for signer in &signers {
            let signer_key: ProtocolSignerVerificationKey =
                key_decode_hex(signer.verification_key.as_ref()).unwrap();
            multisigner
                .register_signer(signer.party_id.to_owned() as ProtocolPartyId, &signer_key)
                .await
                .unwrap();
        }
    }

    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());

    // change the immutable number to alter the beacon
    {
        let new_immutable_number = immutable_file_observer.write().await.increase().unwrap();
        digester
            .set_immutable_file_number(new_immutable_number)
            .await;
        assert_eq!(
            new_immutable_number,
            deps.beacon_provider
                .as_ref()
                .unwrap()
                .read()
                .await
                .get_current_beacon()
                .await
                .unwrap()
                .immutable_file_number
        );
    }
    runtime.cycle().await.unwrap();
    assert_eq!("idle", runtime.get_state());
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());
}
