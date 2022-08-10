use std::{path::PathBuf, sync::Arc, time::Duration};

use mithril_common::{
    chain_observer::FakeObserver,
    digesters::{DumbImmutableDigester, DumbImmutableFileObserver},
    entities::{Beacon, Epoch},
    store::{adapter::MemoryAdapter, StakeStore},
    BeaconProviderImpl,
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
        party_id: "01".to_string(),
        protocol_initializer_store_directory: PathBuf::new(),
        run_interval: 5000,
        stake_store_directory: PathBuf::new(),
    };

    let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
    let chain_observer = Arc::new(FakeObserver::new(None));
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
        beacon_provider,
        certificate_handler,
        chain_observer,
        digester,
        protocol_initializer_store,
        single_signer,
        stake_store,
    };

    let runner = Box::new(SignerRunner::new(config, services));
    let state_machine =
        StateMachine::new(SignerState::Unregistered, runner, Duration::from_secs(5));
}
