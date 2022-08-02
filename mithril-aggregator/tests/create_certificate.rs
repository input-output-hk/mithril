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

/// Simple struct to give a more helpful error message when ticking the state machine
struct TickErrorMessage {
    tick_no: u64,
}

impl TickErrorMessage {
    pub fn new() -> Self {
        Self { tick_no: 0 }
    }

    pub fn get_message(&mut self) -> String {
        self.tick_no += 1;

        format!(
            "Ticking the state machine should not fail (tick n° {})",
            self.tick_no
        )
    }
}

#[tokio::test]
async fn create_certificate() {
    // initialization
    let mut tick_error_msg = TickErrorMessage::new();
    let (mut deps, config) = initialize_dependencies().await;
    let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
    let chain_observer = Arc::new(FakeObserver::new(Some(fake_data::beacon())));
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        mithril_common::CardanoNetwork::TestNet(42),
    ));
    let digester = Arc::new(DumbDigester::default());
    deps.immutable_file_observer = immutable_file_observer.clone();
    deps.beacon_provider = beacon_provider;
    deps.chain_observer = chain_observer.clone();
    deps.digester = digester.clone();
    let deps = Arc::new(deps);
    let runner = Arc::new(AggregatorRunner::new(config.clone(), deps.clone()));
    let mut runtime =
        AggregatorRuntime::new(Duration::from_millis(config.interval), None, runner.clone())
            .await
            .expect("Instantiating the Runtime should not fail.");

    // create signers & declare stake distribution
    let signers = tests_setup::setup_signers(2);
    chain_observer
        .set_signers(
            signers
                .iter()
                .map(|(party_id, stake, verification_key, _, _)| {
                    SignerWithStake::new(
                        party_id.to_owned(),
                        key_encode_hex(verification_key).unwrap(),
                        *stake,
                    )
                })
                .collect(),
        )
        .await;

    // start the runtime state machine
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());

    // register signers
    {
        let mut multisigner = deps.multi_signer.write().await;

        for (party_id, _stakes, verification_key, _signer, _initializer) in &signers {
            multisigner
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .expect("Registering a signer should not fail.");
        }
    }

    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());

    // change the immutable number to alter the beacon
    {
        let new_immutable_number = immutable_file_observer.increase().await.unwrap();
        digester
            .set_immutable_file_number(new_immutable_number)
            .await;
        assert_eq!(
            new_immutable_number,
            deps.beacon_provider
                .get_current_beacon()
                .await
                .expect("Querying the current beacon should not fail.")
                .immutable_file_number
        );
    }
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("idle", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());

    // change the EPOCH 2 times to get the first valid stake distribution

    // first EPOCH change
    let _epoch = chain_observer
        .next_epoch()
        .await
        .expect("we should get a new epoch");
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("idle", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());

    // second EPOCH change
    let _epoch = chain_observer
        .next_epoch()
        .await
        .expect("we should get a new epoch");
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("idle", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("signing", runtime.get_state());

    // signers send their single signature
    {
        let mut multisigner = deps.multi_signer.write().await;
        let message = multisigner
            .get_current_message()
            .await
            .expect("There should be a message to be signed.");

        for (party_id, _stakes, _verification_key, protocol_signer, _initializer) in &signers {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                let single_signatures = SingleSignatures::new(
                    party_id.to_string(),
                    key_encode_hex(&signature).expect("hex encoding should not fail"),
                    signature.indexes,
                );

                multisigner
                    .register_single_signature(&single_signatures)
                    .await
                    .expect("registering a winning lottery signature should not fail");
            }
        }
    }

    // The state machine should issue a multisignature
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("idle", runtime.get_state());
    let last_certificates = deps
        .certificate_store
        .get_list(5)
        .await
        .expect("Querying certificate store should not fail");

    assert_eq!(1, last_certificates.len());
    runtime.cycle().await.expect(&tick_error_msg.get_message());
    assert_eq!("idle", runtime.get_state());
}
