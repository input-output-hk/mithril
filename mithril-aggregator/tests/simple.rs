mod init;

use crate::init::RuntimeTester;
use mithril_common::fake_data;

#[tokio::test]
async fn simple_scenario() {
    let mut tester = RuntimeTester::build().await;
    tester
        .deps
        .init_protocol_parameter_store(fake_data::protocol_parameters())
        .await;

    if let Err(e) = tester.runtime.cycle().await {
        panic!("FIRST CYCLE FAILED: {:?}", e);
    }

    assert_eq!("signing", tester.runtime.get_state());
}
