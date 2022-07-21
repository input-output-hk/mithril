mod init;
use init::initialize_dependencies;
use mithril_aggregator::{
    AggregatorRunner, AggregatorRuntime, BeaconProviderImpl, DumbImmutableFileObserver,
};
use mithril_common::chain_observer::FakeObserver;
use mithril_common::crypto_helper::{key_encode_hex, tests_setup};
use mithril_common::digesters::DumbDigester;
use mithril_common::entities::{SignerWithStake, SingleSignatures};
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
    let _ = chain_observer.write().await.current_beacon = Some(fake_data::beacon());
    let beacon_provider = Arc::new(RwLock::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        mithril_common::CardanoNetwork::TestNet(42),
    )));
    let digester = Arc::new(DumbDigester::default());
    deps.with_immutable_file_observer(immutable_file_observer.clone())
        .with_beacon_provider(beacon_provider)
        .with_chain_observer(chain_observer.clone())
        .with_digester(digester.clone());
    let deps = Arc::new(deps);
    let runner = Arc::new(AggregatorRunner::new(config.clone(), deps.clone()));
    let mut runtime =
        AggregatorRuntime::new(Duration::from_millis(config.interval), None, runner.clone())
            .await
            .unwrap();

    // create signers & declare stake distribution
    let signers = tests_setup::setup_signers(2);
    let _ = chain_observer.write().await.signers = signers
        .iter()
        .map(|(party_id, stake, verification_key, _, _)| {
            SignerWithStake::new(
                party_id.to_owned(),
                key_encode_hex(verification_key).unwrap(),
                *stake,
            )
        })
        .collect();

    // start the runtime state machine
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.unwrap();
    assert_eq!("signing", runtime.get_state());

    // register signers
    {
        let mut multisigner = deps.multi_signer.as_ref().unwrap().write().await;

        for (party_id, _stakes, verification_key, _signer, _initializer) in &signers {
            multisigner
                .register_signer(party_id.to_owned(), verification_key)
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

    {
        let multisigner = deps.multi_signer.as_ref().unwrap().write().await;
        let _protocol_parameters = deps
            .certificate_pending_store
            .as_ref()
            .unwrap()
            .read()
            .await
            .get()
            .await
            .unwrap()
            .unwrap()
            .protocol_parameters;
        let message = multisigner.get_current_message().await.unwrap();

        for (party_id, _stakes, _verification_key, protocol_signer, _initializer) in &signers {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                let _single_signatures = SingleSignatures::new(
                    party_id.to_string(),
                    signature.sigma.to_string(),
                    signature.indexes,
                );
                /* TODO: fix why there is no available clerk
                * it sounds there is not stake distribution
                * have to find out why

                    let _res = multisigner
                        .register_single_signature(&single_signatures)
                        .await
                        .expect("registering a winning lottery signature should not fail");
                */
            }
        }
    }
}
