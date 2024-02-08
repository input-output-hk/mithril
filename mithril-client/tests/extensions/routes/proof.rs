use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use crate::extensions::routes::middleware::with_calls_middleware;
use warp::Filter;

pub fn routes(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    proof_cardano_transaction(calls.clone(), returned_value)
}

/// Route: /proof/cardano-transaction
fn proof_cardano_transaction(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("proof" / "cardano-transaction")
        .and(warp::get())
        .and(warp::path::full().map(move |p| p))
        .and(warp::query::raw())
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, query, calls| {
            FakeAggregator::store_call_with_query_and_return_value(
                fullpath,
                query,
                calls,
                returned_value.clone(),
            )
        })
}
