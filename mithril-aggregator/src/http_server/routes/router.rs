use crate::http_server::routes::{
    artifact_routes, certificate_routes, epoch_routes, root_routes, signatures_routes,
    signer_routes, statistics_routes,
};
use crate::http_server::SERVER_BASE_PATH;
use crate::DependencyContainer;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::MITHRIL_API_VERSION_HEADER;

use reqwest::StatusCode;
use slog_scope::warn;
use std::sync::Arc;
use warp::http::Method;
use warp::reject::Reject;
use warp::{Filter, Rejection, Reply};

use super::middlewares;

#[derive(Debug)]
pub struct VersionMismatchError;

impl Reject for VersionMismatchError {}

#[derive(Debug)]
pub struct VersionParseError;

impl Reject for VersionParseError {}

/// Routes
pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec!["content-type"])
        .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

    warp::any()
        .and(header_must_be(
            dependency_manager.api_version_provider.clone(),
        ))
        .and(warp::path(SERVER_BASE_PATH))
        .and(
            certificate_routes::routes(dependency_manager.clone())
                .or(artifact_routes::snapshot::routes(
                    dependency_manager.clone(),
                ))
                .or(artifact_routes::mithril_stake_distribution::routes(
                    dependency_manager.clone(),
                ))
                .or(signer_routes::routes(dependency_manager.clone()))
                .or(signatures_routes::routes(dependency_manager.clone()))
                .or(epoch_routes::routes(dependency_manager.clone()))
                .or(statistics_routes::routes(dependency_manager.clone()))
                .or(root_routes::routes(dependency_manager.clone()))
                .with(cors),
        )
        .recover(handle_custom)
        .and(middlewares::with_api_version_provider(dependency_manager))
        .map(|reply, api_version_provider: Arc<APIVersionProvider>| {
            warp::reply::with_header(
                reply,
                MITHRIL_API_VERSION_HEADER,
                &api_version_provider
                    .compute_current_version()
                    .unwrap()
                    .to_string(),
            )
        })
}

/// API Version verification
fn header_must_be(
    api_version_provider: Arc<APIVersionProvider>,
) -> impl Filter<Extract = (), Error = Rejection> + Clone {
    warp::header::optional(MITHRIL_API_VERSION_HEADER)
        .and(warp::any().map(move || api_version_provider.clone()))
        .and_then(
            move |maybe_header: Option<String>, api_version_provider: Arc<APIVersionProvider>| async move {
                match maybe_header {
                    None => Ok(()),
                    Some(version) => match semver::Version::parse(&version) {
                        Ok(version)
                            if (api_version_provider
                                .compute_current_version_requirement().unwrap()
                                .matches(&version))
                            .to_owned() =>
                        {
                            Ok(())
                        }
                        Ok(_version) => Err(warp::reject::custom(VersionMismatchError)),
                        Err(err) => {
                            warn!("⇄ HTTP SERVER::api_version_check::parse_error"; "error" => ?err);
                            Err(warp::reject::custom(VersionParseError))
                        }
                    },
                }
            },
        )
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
    use std::collections::HashMap;

    use mithril_common::{
        entities::Epoch,
        era::{EraChecker, SupportedEra},
    };

    use super::*;

    #[tokio::test]
    async fn test_no_version() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let api_version_provider = Arc::new(APIVersionProvider::new(Arc::new(era_checker)));
        let filters = header_must_be(api_version_provider);
        warp::test::request()
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect("request without a version in headers should not be rejected");
    }

    #[tokio::test]
    async fn test_parse_version_error() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let api_version_provider = Arc::new(APIVersionProvider::new(Arc::new(era_checker)));
        let filters = header_must_be(api_version_provider);
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
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "1.0.0".to_string());
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);
        let filters = header_must_be(api_version_provider);
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, "0.0.999")
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect_err(r#"request with bad version "0.0.999" should be rejected with a version mismatch error"#);
    }

    #[tokio::test]
    async fn test_good_version() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "0.1.0".to_string());
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);
        let filters = header_must_be(api_version_provider);
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, "0.1.2")
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect(r#"request with the good version "0.1.2" should not be rejected"#);
    }
}
