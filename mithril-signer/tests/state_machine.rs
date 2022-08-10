use std::{path::PathBuf, sync::Arc, time::Duration};

use mithril_common::{
    chain_observer::FakeObserver,
    digesters::{DumbImmutableDigester, DumbImmutableFileObserver},
    entities::{Beacon, Epoch},
    fake_data,
    store::{adapter::MemoryAdapter, StakeStore},
    BeaconProvider, BeaconProviderImpl,
};
use mithril_signer::{
    Config, DumbCertificateHandler, MithrilSingleSigner, ProtocolInitializerStore, SignerRunner,
    SignerServices, SignerState, StateMachine,
};

#[tokio::test]
async fn test_create_single_signature() {
    let config = Config {
        aggregator_endpoint: "http://0.0.0.0:8000".to_string(),
        cardano_cli_path: PathBuf::new(),
        cardano_node_socket_path: PathBuf::new(),
        db_directory: PathBuf::new(),
        network: "devnet".to_string(),
        network_magic: Some(42),
        party_id: "1".to_string(),
        protocol_initializer_store_directory: PathBuf::new(),
        run_interval: 5000,
        stake_store_directory: PathBuf::new(),
    };

    let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
    let chain_observer = Arc::new(FakeObserver::new(Some(Beacon {
        epoch: Epoch(1),
        immutable_file_number: 1,
        network: "devnet".to_string(),
    })));
    chain_observer
        .set_signers(fake_data::signers_with_stakes(4))
        .await;
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_observer.clone(),
        config.get_network().unwrap(),
    ));
    let certificate_handler = Arc::new(DumbCertificateHandler::new());
    let digester = Arc::new(DumbImmutableDigester::new("DIGEST", true));
    let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let single_signer = Arc::new(MithrilSingleSigner::new(config.party_id.clone()));
    let stake_store = Arc::new(StakeStore::new(Box::new(MemoryAdapter::new(None).unwrap())));

    let services = SignerServices {
        beacon_provider: beacon_provider.clone(),
        certificate_handler: certificate_handler.clone(),
        chain_observer: chain_observer.clone(),
        digester: digester.clone(),
        protocol_initializer_store: protocol_initializer_store.clone(),
        single_signer: single_signer.clone(),
        stake_store: stake_store.clone(),
    };

    let runner = Box::new(SignerRunner::new(config, services));

    // 1 the state machine is and remains in UNREGISTERED state until a certificate pending is got
    let mut state_machine =
        StateMachine::new(SignerState::Unregistered, runner, Duration::from_secs(5));

    let cycle_error_msg = "ticking the state machine should not fail".to_string();
    state_machine.cycle().await.expect(&cycle_error_msg);
    assert!(state_machine.get_state().is_unregistered());
    state_machine.cycle().await.expect(&cycle_error_msg);
    assert!(state_machine.get_state().is_unregistered());

    // increasing immutable files should not change the state
    immutable_observer.shall_return(Some(1)).await;
    state_machine.cycle().await.expect(&cycle_error_msg);
    assert!(state_machine.get_state().is_unregistered());

    // changing the epoch should not change the state
    chain_observer.next_epoch().await;
    state_machine.cycle().await.expect(&cycle_error_msg);
    assert!(state_machine.get_state().is_unregistered());

    // getting a certificate pending changes the state
    let beacon = beacon_provider
        .get_current_beacon()
        .await
        .expect("beacon provider should not fail");
    let mut certificate_pending = fake_data::certificate_pending();
    certificate_pending.beacon = beacon;
    certificate_handler
        .set_certificate_pending(Some(certificate_pending))
        .await;
    state_machine.cycle().await.expect(&cycle_error_msg);
    assert!(state_machine.get_state().is_registered());
}
