use super::middleware::with_calls_middleware;
use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use warp::Filter;

pub fn routes(
    calls: FakeAggregatorCalls,
    stake_distributions_returned_value: String,
    stake_distribution_by_id_returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    mithril_stake_distributions(calls.clone(), stake_distributions_returned_value).or(
        mithril_stake_distribution_by_id(calls, stake_distribution_by_id_returned_value),
    )
}

/// Route: /artifact/mithril-stake-distributions
fn mithril_stake_distributions(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distributions")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}

/// Route: /artifact/mithril-stake-distribution/:id
fn mithril_stake_distribution_by_id(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distribution" / String)
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |_param, fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}
