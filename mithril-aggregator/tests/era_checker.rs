mod test_extensions;
use mithril_aggregator::{Configuration, RuntimeError};
use mithril_common::{
    chain_observer::ChainObserver,
    entities::{Epoch, ProtocolParameters},
    era::{EraMarker, SupportedEra},
    test_utils::MithrilFixtureBuilder,
};

use test_extensions::{utilities::get_test_dir, RuntimeTester};

// NOTE: Due to the shared nature of the Logger, there cannot be two methods in
// the same test file. Because the logger is wiped of memory when the first
// methods terminates it also removes the other method's logger from memory.
#[tokio::test]
async fn testing_eras() {
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.95,
    };
    let configuration = Configuration {
        protocol_parameters: protocol_parameters.clone(),
        data_stores_directory: get_test_dir("testing_eras").join("aggregator.sqlite3"),
        ..Configuration::new_sample()
    };
    let mut tester = RuntimeTester::build(configuration).await;
    tester.era_reader_adapter.set_markers(vec![
        EraMarker::new("unsupported", Some(Epoch(0))),
        EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(12))),
    ]);
    comment!("Starting the runtime at unsupported Era.");
    if let Err(e) = tester.runtime.cycle().await {
        match e {
            RuntimeError::Critical {
                message: _,
                nested_error: _,
            } => {}
            _ => panic!("Expected a Critical Error, got {e:?}."),
        }
    } else {
        panic!("Expected an error, got Ok().")
    }

    // Testing the Era changes during the process
    let protocol_parameters = ProtocolParameters {
        k: 5,
        m: 100,
        phi_f: 0.95,
    };
    tester.era_reader_adapter.set_markers(vec![
        EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(0))),
        EraMarker::new("unsupported", Some(Epoch(11))),
    ]);
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(5)
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
            signers_with_stake.clone(),
            &protocol_parameters,
        )
        .await;
    let _ = tester
        .chain_observer
        .get_current_epoch()
        .await
        .unwrap()
        .unwrap();

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&signers).await.unwrap();

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("start the runtime state machine");
    cycle!(tester, "ready");

    // reach unsupported Epoch
    let current_epoch = tester
        .chain_observer
        .next_epoch()
        .await
        .expect("Epoch was expected to be 11.");
    assert_eq!(11, current_epoch);
    cycle!(tester, "idle");

    if let Err(e) = tester.runtime.cycle().await {
        match e {
            RuntimeError::Critical {
                message: _,
                nested_error: _,
            } => {}
            _ => panic!("Expected a Critical Error, got {e:?}."),
        }
    } else {
        panic!("Expected an error, got Ok().")
    }
}
