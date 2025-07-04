use std::convert::Infallible;
use std::sync::Arc;

use serde_json::json;
use warp::Filter;
use warp::http::StatusCode;

use mithril_common::StdResult;
use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_test_http_server::{TestHttpServer, test_http_server};

use crate::test_extensions::{AggregatorObserver, RuntimeTester};

pub struct LeaderAggregatorHttpServer {}

impl LeaderAggregatorHttpServer {
    pub fn spawn(runtime_tester: &RuntimeTester) -> StdResult<TestHttpServer> {
        let routes = warp::path("epoch-settings")
            .and(with_observer(runtime_tester))
            .and_then(epoch_settings_handler);

        Ok(test_http_server(routes))
    }
}

fn with_observer(
    runtime_tester: &RuntimeTester,
) -> impl Filter<Extract = (Arc<AggregatorObserver>,), Error = Infallible> + Clone + use<> {
    let observer = runtime_tester.observer.clone();
    warp::any().map(move || observer.clone())
}

async fn epoch_settings_handler(
    observer: Arc<AggregatorObserver>,
) -> Result<impl warp::Reply, Infallible> {
    let allowed_discriminants = SignedEntityTypeDiscriminants::all();
    let epoch_settings_message = observer.get_epoch_settings(allowed_discriminants).await;
    match epoch_settings_message {
        Ok(message) => Ok(Box::new(warp::reply::with_status(
            warp::reply::json(&message),
            StatusCode::OK,
        ))),
        Err(err) => Ok(Box::new(warp::reply::with_status(
            warp::reply::json(&json!(err.to_string())),
            StatusCode::INTERNAL_SERVER_ERROR,
        ))),
    }
}
