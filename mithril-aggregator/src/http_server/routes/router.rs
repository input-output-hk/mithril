use crate::http_server::routes::{
    certificate_routes, epoch_routes, signatures_routes, signer_routes, snapshot_routes,
};
use crate::http_server::SERVER_BASE_PATH;
use crate::DependencyManager;

use mithril_common::MITHRIL_API_VERSION;

use reqwest::header::{HeaderMap, HeaderValue};
use std::sync::Arc;
use warp::http::Method;
use warp::{Filter, Rejection};

/// Routes
pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec!["content-type"])
        .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);
    let mut headers = HeaderMap::new();
    headers.insert(
        "mithril-api-version",
        HeaderValue::from_static(MITHRIL_API_VERSION),
    );
    warp::any()
        .and(header_must_be())
        .and(warp::path(SERVER_BASE_PATH))
        .and(
            certificate_routes::routes(dependency_manager.clone())
                .or(snapshot_routes::routes(dependency_manager.clone()))
                .or(signer_routes::routes(dependency_manager.clone()))
                .or(signatures_routes::routes(dependency_manager.clone()))
                .or(epoch_routes::routes(dependency_manager))
                .with(cors)
                .with(warp::reply::with::headers(headers)),
        )
}

/// API Version verification
fn header_must_be() -> impl Filter<Extract = (), Error = Rejection> + Copy {
    warp::header::optional("mithril-api-version")
        .and_then(|maybe_header: Option<String>| async move {
            match maybe_header {
                None => Ok(()),
                Some(version) if version == MITHRIL_API_VERSION => Ok(()),
                Some(_version) => Err(warp::reject()),
            }
        })
        .untuple_one()
}
