use super::middleware::with_calls_middleware;
use crate::extensions::fake::{FakeAggregator, FakeAggregatorCalls};
use warp::Filter;

pub fn routes(
    calls: FakeAggregatorCalls,
    certificate_certificates_returned_value: Option<String>,
    certificate_certificate_hash_returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    certificate_certificates(
        calls.clone(),
        certificate_certificates_returned_value.unwrap_or_default(),
    )
    .or(certificate_certificate_hash(
        calls,
        certificate_certificate_hash_returned_value,
    ))
}

/// Route: /certificates
fn certificate_certificates(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificates")
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}

/// Route: /certificate/{certificate_hash}
fn certificate_certificate_hash(
    calls: FakeAggregatorCalls,
    returned_value: String,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificate" / String)
        .and(warp::path::full().map(move |p| p))
        .and(with_calls_middleware(calls.clone()))
        .and_then(move |_param, fullpath, calls| {
            FakeAggregator::store_call_and_return_value(fullpath, calls, returned_value.clone())
        })
}
