use std::collections::HashMap;
use std::sync::Arc;

use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
use mithril_common::store::adapter::DumbStoreAdapter;
use mithril_common::store::StakeStore;
use mithril_common::{chain_observer::FakeObserver, store::StakeStorer};
use mithril_common::{fake_data, BeaconProviderImpl, CardanoNetwork};

use mithril_signer::{DumbCertificateHandler, MithrilSingleSigner, SignerServices};

fn init_services() -> SignerServices {
    let chain_observer = Arc::new(FakeObserver::default());
    SignerServices {
        stake_store: Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()))),
        certificate_handler: Arc::new(DumbCertificateHandler::default()),
        chain_observer: chain_observer.clone(),
        digester: Arc::new(DumbImmutableDigester::new("whatever", true)),
        single_signer: Arc::new(MithrilSingleSigner::new("PARTY_01".to_string(), "whatever")),
        beacon_provider: Arc::new(BeaconProviderImpl::new(
            chain_observer,
            Arc::new(DumbImmutableFileObserver::default()),
            CardanoNetwork::TestNet(42),
        )),
    }
}
#[tokio::test]
async fn using_services() {
    let mut services = init_services();

    // overload a service
    let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new())));
    services.stake_store = stake_store.clone();

    let stake_store_service = services.stake_store;
    stake_store
        .save_stakes(
            1,
            HashMap::from_iter(
                fake_data::signers_with_stakes(1)
                    .into_iter()
                    .map(|s| (s.party_id, s.stake)),
            ),
        )
        .await
        .expect("saving a signer should not fail");
    let signers = stake_store_service
        .get_stakes(1)
        .await
        .expect("test adapter should not fail")
        .expect("query should not return empty set");
    assert_eq!(1, signers.len());
}
