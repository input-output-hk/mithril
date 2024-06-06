mod test_extensions;
use mithril_aggregator::{Configuration, RuntimeError};
use mithril_common::{
    entities::{ChainPoint, Epoch, ProtocolParameters, TimePoint},
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
        data_stores_directory: get_test_dir("testing_eras"),
        ..Configuration::new_sample()
    };
    let mut tester = RuntimeTester::build(
        TimePoint::new(1, 1, ChainPoint::new(10, 1, "block_hash-1")),
        configuration,
    )
    .await;
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
        EraMarker::new("unsupported", Some(Epoch(2))),
    ]);
    let fixture = MithrilFixtureBuilder::default()
        .with_signers(5)
        .with_protocol_parameters(protocol_parameters.clone())
        .build();
    tester.init_state_from_fixture(&fixture).await.unwrap();

    comment!("Boostrap the genesis certificate");
    tester.register_genesis_certificate(&fixture).await.unwrap();

    comment!("Increase immutable number");
    tester.increase_immutable_number().await.unwrap();

    comment!("start the runtime state machine");
    cycle!(tester, "ready");

    // reach unsupported Epoch
    let current_epoch = tester.chain_observer.next_epoch().await.unwrap();
    assert_eq!(2, current_epoch, "Epoch was expected to be 2.");
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
