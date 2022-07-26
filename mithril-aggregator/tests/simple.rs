mod init;
use std::sync::Arc;

use init::initialize_dependencies;
use mithril_aggregator::{AggregatorRunner, AggregatorRuntime};
use tokio::time::Duration;

#[tokio::test]
async fn simple_scenario() {
    let (deps, config) = initialize_dependencies().await;
    let mut runtime = AggregatorRuntime::new(
        Duration::from_millis(config.interval),
        None,
        Arc::new(AggregatorRunner::new(config, Arc::new(deps))),
    )
    .await
    .unwrap();

    if let Err(e) = runtime.cycle().await {
        panic!("FIRST CYCLE FAILED: {:?}", e);
    }

    assert_eq!("signing", runtime.get_state());
}
