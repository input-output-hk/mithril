use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use crate::extensions::routes::middleware::with_calls_middleware;
use std::collections::HashMap;
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
        .and(warp::query::<HashMap<String, String>>())
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |_query, fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}
