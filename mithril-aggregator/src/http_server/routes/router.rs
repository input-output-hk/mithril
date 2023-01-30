use crate::http_server::routes::{
    certificate_routes, epoch_routes, signatures_routes, signer_routes, snapshot_routes,
};
use crate::http_server::SERVER_BASE_PATH;
use crate::DependencyManager;

use mithril_common::{
    MITHRIL_API_VERSION, MITHRIL_API_VERSION_HEADER, MITHRIL_API_VERSION_REQUIREMENT,
};

use reqwest::header::{HeaderMap, HeaderValue};
use reqwest::StatusCode;
use slog_scope::warn;
use std::sync::Arc;
use warp::http::Method;
use warp::reject::Reject;
use warp::{Filter, Rejection, Reply};

#[derive(Debug)]
pub struct VersionMismatchError;

impl Reject for VersionMismatchError {}

#[derive(Debug)]
pub struct VersionParseError;

impl Reject for VersionParseError {}

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
        MITHRIL_API_VERSION_HEADER,
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
                .with(cors),
        )
        .recover(handle_custom)
        .with(warp::reply::with::headers(headers))
}

/// API Version verification
fn header_must_be() -> impl Filter<Extract = (), Error = Rejection> + Copy {
    warp::header::optional(MITHRIL_API_VERSION_HEADER)
        .and_then(|maybe_header: Option<String>| async move {
            match maybe_header {
                None => Ok(()),
                Some(version) => match semver::Version::parse(&version) {
                    Ok(version) if MITHRIL_API_VERSION_REQUIREMENT.matches(&version) => Ok(()),
                    Ok(_version) => Err(warp::reject::custom(VersionMismatchError)),
                    Err(err) => {
                        warn!("⇄ HTTP SERVER::api_version_check::parse_error"; "error" => ?err);
                        Err(warp::reject::custom(VersionParseError))
                    }
                },
            }
        })
        .untuple_one()
}

pub async fn handle_custom(reject: Rejection) -> Result<impl Reply, Rejection> {
    if reject.find::<VersionMismatchError>().is_some() {
        Ok(StatusCode::PRECONDITION_FAILED)
    } else {
        Err(reject)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_no_version() {
        let filters = header_must_be();
        warp::test::request()
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect("request without a version in headers should not be rejected");
    }

    #[tokio::test]
    async fn test_parse_version_error() {
        let filters = header_must_be();
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, "not_a_version")
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect_err(
                r#"request with an unparsable version should be rejected with a version parse error"#,
            );
    }

    #[tokio::test]
    async fn test_bad_version() {
        let filters = header_must_be();
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, "0.0.999")
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect_err(r#"request with bad version "0.0.999" should be rejected with a version mismatch error"#);
    }

    #[tokio::test]
    async fn test_good_version() {
        let filters = header_must_be();
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, MITHRIL_API_VERSION)
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect("request with the current api version should not be rejected");
    }
}
