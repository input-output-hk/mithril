mod test_extensions;

use mithril_common::fake_data;
use test_extensions::RuntimeTester;

#[tokio::test]
async fn simple_scenario() {
    let mut tester = RuntimeTester::build(fake_data::protocol_parameters()).await;
    tester
        .deps
        .init_protocol_parameter_store(&fake_data::protocol_parameters())
        .await;

    if let Err(e) = tester.runtime.cycle().await {
        panic!("FIRST CYCLE FAILED: {:?}", e);
    }

    assert_eq!("signing", tester.runtime.get_state());
}
