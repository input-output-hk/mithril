use super::middleware::with_calls_middleware;
use crate::extensions::fake_aggregator::{FakeAggregator, FakeAggregatorCalls};
use warp::Filter;

pub fn routes(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    add_statistics(calls.clone())
        .or(add_statistics_immutable_files_restored(calls.clone()))
        .or(add_statistics_ancillary_files_restored(calls.clone()))
        .or(add_statistics_partial_restoration(calls.clone()))
}

/// Route: /statistics/snapshot
fn add_statistics(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "snapshot")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, "".to_string())
        })
}

/// Route: /statistics/cardano-database/immutable-files-restored
fn add_statistics_immutable_files_restored(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "immutable-files-restored")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, "".to_string())
        })
}

/// Route: /statistics/cardano-database/ancillary-files-restored
fn add_statistics_ancillary_files_restored(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "ancillary-files-restored")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, "".to_string())
        })
}

/// Route: /statistics/cardano-database/partial-restoration
fn add_statistics_partial_restoration(
    calls: FakeAggregatorCalls,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "partial-restoration")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, "".to_string())
        })
}
