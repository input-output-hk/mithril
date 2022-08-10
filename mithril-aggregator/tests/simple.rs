mod init;

use crate::init::RuntimeTester;

#[tokio::test]
async fn simple_scenario() {
    let mut tester = RuntimeTester::build().await;

    if let Err(e) = tester.runtime.cycle().await {
        panic!("FIRST CYCLE FAILED: {:?}", e);
    }

    assert_eq!("signing", tester.runtime.get_state());
}
