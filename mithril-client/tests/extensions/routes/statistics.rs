use super::middleware::with_calls_middleware;
use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use warp::Filter;

pub fn routes(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    add_statistics(calls.clone())
}

/// Route: /statistics/snapshot
fn add_statistics(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "snapshot")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(vec![], fullpath, calls, "".to_string())
        })
}
