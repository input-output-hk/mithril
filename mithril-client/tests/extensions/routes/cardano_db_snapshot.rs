use std::sync::{Arc, RwLock};

use warp::Filter;

use mithril_client::CardanoDatabaseSnapshot;

use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};

use super::middleware::with_calls_middleware;

pub fn routes(
    calls: FakeAggregatorCalls,
    cardano_db_snapshots_returned_value: String,
    cardano_db_snapshot_by_id_returned_value: Arc<RwLock<CardanoDatabaseSnapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    cardano_db_snapshots(calls.clone(), cardano_db_snapshots_returned_value).or(
        cardano_db_snapshot_by_id(calls.clone(), cardano_db_snapshot_by_id_returned_value),
    )
}

/// Route: /artifact/cardano-database
fn cardano_db_snapshots(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}

/// Route: /artifact/cardano-database/:hash
fn cardano_db_snapshot_by_id(
    calls: FakeAggregatorCalls,
    returned_value: Arc<RwLock<CardanoDatabaseSnapshot>>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database" / String)
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |_param, fullpath, calls| {
            let data = returned_value.read().unwrap();
            FakeAggregator::store_call_and_return_value(
                fullpath,
                calls,
                serde_json::to_string(&data.clone()).unwrap(),
            )
        })
}
