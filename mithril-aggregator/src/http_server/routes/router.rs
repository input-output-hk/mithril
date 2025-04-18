use crate::http_server::routes::{
    artifact_routes, certificate_routes, epoch_routes, http_server_child_logger, root_routes,
    signatures_routes, signer_routes, statistics_routes, status,
};
use crate::http_server::SERVER_BASE_PATH;
use crate::tools::url_sanitizer::SanitizedUrlWithTrailingSlash;
use crate::DependencyContainer;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_common::{
    CardanoNetwork, StmAggrSigType, MITHRIL_API_VERSION_HEADER, MITHRIL_ORIGIN_TAG_HEADER,
};

use slog::{warn, Logger};
use std::collections::{BTreeSet, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use warp::http::Method;
use warp::http::StatusCode;
use warp::reject::Reject;
use warp::{Filter, Rejection, Reply};

use super::{middlewares, proof_routes};

#[derive(Debug)]
pub struct VersionMismatchError;

impl Reject for VersionMismatchError {}

#[derive(Debug)]
pub struct VersionParseError;

impl Reject for VersionParseError {}

/// HTTP Server configuration
pub struct RouterConfig {
    pub network: CardanoNetwork,
    pub server_url: SanitizedUrlWithTrailingSlash,
    pub allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    pub cardano_transactions_prover_max_hashes_allowed_by_request: usize,
    pub cardano_db_artifacts_directory: PathBuf,
    pub snapshot_directory: PathBuf,
    pub cardano_node_version: String,
    pub allow_http_serve_directory: bool,
    pub origin_tag_white_list: HashSet<String>,
    pub aggregation_type: StmAggrSigType,
}

#[cfg(test)]
impl RouterConfig {
    pub fn dummy() -> Self {
        Self {
            network: CardanoNetwork::DevNet(87),
            server_url: SanitizedUrlWithTrailingSlash::parse("http://0.0.0.0:8000/").unwrap(),
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ]),
            cardano_transactions_prover_max_hashes_allowed_by_request: 1_000,
            cardano_db_artifacts_directory: PathBuf::from("/dummy/cardano-db/directory"),
            snapshot_directory: PathBuf::from("/dummy/snapshot/directory"),
            cardano_node_version: "1.2.3".to_string(),
            allow_http_serve_directory: false,
            origin_tag_white_list: HashSet::from(["DUMMY_TAG".to_string()]),
            aggregation_type: StmAggrSigType::StmAggrSigConcatenation,
        }
    }

    pub fn dummy_with_origin_tag_white_list(origin_tag_white_list: &[&str]) -> Self {
        Self {
            origin_tag_white_list: origin_tag_white_list
                .iter()
                .map(|tag| tag.to_string())
                .collect(),
            ..RouterConfig::dummy()
        }
    }
}

/// Shared state for the router
pub struct RouterState {
    pub dependencies: Arc<DependencyContainer>,
    pub configuration: RouterConfig,
}

impl RouterState {
    /// `RouterState` factory
    pub fn new(dependencies: Arc<DependencyContainer>, configuration: RouterConfig) -> Self {
        Self {
            dependencies,
            configuration,
        }
    }
}

#[cfg(test)]
impl RouterState {
    pub fn new_with_dummy_config(dependencies: Arc<DependencyContainer>) -> Self {
        Self {
            dependencies,
            configuration: RouterConfig::dummy(),
        }
    }

    pub fn new_with_origin_tag_white_list(
        dependencies: Arc<DependencyContainer>,
        origin_tag_white_list: &[&str],
    ) -> Self {
        Self {
            dependencies,
            configuration: RouterConfig::dummy_with_origin_tag_white_list(origin_tag_white_list),
        }
    }
}

/// Routes
pub fn routes(
    state: Arc<RouterState>,
) -> impl Filter<Extract = (impl Reply,), Error = Rejection> + Clone {
    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec![
            "content-type",
            MITHRIL_API_VERSION_HEADER,
            MITHRIL_ORIGIN_TAG_HEADER,
        ])
        .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

    warp::any()
        .and(header_must_be(
            state.dependencies.api_version_provider.clone(),
            http_server_child_logger(&state.dependencies.root_logger),
        ))
        .and(warp::path(SERVER_BASE_PATH))
        .and(
            certificate_routes::routes(&state)
                .or(artifact_routes::snapshot::routes(&state))
                .or(artifact_routes::cardano_database::routes(&state))
                .or(artifact_routes::mithril_stake_distribution::routes(&state))
                .or(artifact_routes::cardano_stake_distribution::routes(&state))
                .or(artifact_routes::cardano_transaction::routes(&state))
                .or(proof_routes::routes(&state))
                .or(signer_routes::routes(&state))
                .or(signatures_routes::routes(&state))
                .or(epoch_routes::routes(&state))
                .or(statistics_routes::routes(&state))
                .or(root_routes::routes(&state))
                .or(status::routes(&state)),
        )
        .recover(handle_custom)
        .and(middlewares::with_api_version_provider(&state))
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
        .with(cors)
        .with(middlewares::log_route_call(&state))
}

/// API Version verification
fn header_must_be(
    api_version_provider: Arc<APIVersionProvider>,
    logger: Logger,
) -> impl Filter<Extract = (), Error = Rejection> + Clone {
    warp::header::optional(MITHRIL_API_VERSION_HEADER)
        .and(warp::any().map(move || api_version_provider.clone()))
        .and(warp::any().map(move || logger.clone()))
        .and_then(
            move |maybe_header: Option<String>,
                  api_version_provider: Arc<APIVersionProvider>,
                  logger: Logger| async move {
                match maybe_header {
                    None => Ok(()),
                    Some(version) => match semver::Version::parse(&version) {
                        Ok(version)
                            if api_version_provider
                                .compute_current_version_requirement()
                                .unwrap()
                                .matches(&version)
                                .to_owned() =>
                        {
                            Ok(())
                        }
                        Ok(_version) => Err(warp::reject::custom(VersionMismatchError)),
                        Err(err) => {
                            warn!(logger, "api_version_check::parse_error"; "error" => ?err);
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
    } else if reject.is_not_found() {
        Ok(StatusCode::NOT_FOUND)
    } else {
        Err(reject)
    }
}

#[cfg(test)]
mod tests {
    use semver::Version;
    use std::collections::HashMap;
    use warp::test::RequestBuilder;

    use mithril_common::{
        entities::Epoch,
        era::{EraChecker, SupportedEra},
        MITHRIL_ORIGIN_TAG_HEADER,
    };

    use crate::initialize_dependencies;
    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn test_no_version() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let api_version_provider = Arc::new(APIVersionProvider::new(Arc::new(era_checker)));
        let filters = header_must_be(api_version_provider, TestLogger::stdout());
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
        let filters = header_must_be(api_version_provider, TestLogger::stdout());
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
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(1, 0, 0));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);
        let filters = header_must_be(api_version_provider, TestLogger::stdout());
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
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(0, 1, 0));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);
        let filters = header_must_be(api_version_provider, TestLogger::stdout());
        warp::test::request()
            .header(MITHRIL_API_VERSION_HEADER, "0.1.2")
            .path("/aggregator/whatever")
            .filter(&filters)
            .await
            .expect(r#"request with the good version "0.1.2" should not be rejected"#);
    }

    #[tokio::test]
    async fn test_404_response_should_include_status_code_and_headers() {
        let container = Arc::new(initialize_dependencies!().await);
        let state = RouterState::new_with_dummy_config(container);
        let routes = routes(Arc::new(state));

        let response = warp::test::request()
            .path("/aggregator/a-route-that-does-not-exist")
            // We need to set the Origin header to trigger the CORS middleware
            .header("Origin", "http://localhost")
            .reply(&routes)
            .await;
        let response_headers = response.headers();

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        assert!(
            response_headers.get(MITHRIL_API_VERSION_HEADER).is_some(),
            "API version header should be present, headers: {response_headers:?}",
        );
        assert!(
            response_headers
                .get("access-control-allow-origin")
                .is_some(),
            "CORS headers should be present, headers: {response_headers:?}",
        );
    }

    #[tokio::test]
    async fn test_authorized_request_headers() {
        fn request_with_access_control_request_headers(headers: String) -> RequestBuilder {
            warp::test::request()
                .method("OPTIONS")
                .path("/aggregator")
                // We need to set the Origin header to trigger the CORS middleware
                .header("Origin", "http://localhost")
                .header("access-control-request-method", "GET")
                .header("access-control-request-headers", headers)
        }

        let container = Arc::new(initialize_dependencies!().await);
        let state = RouterState::new_with_dummy_config(container);
        let routes = routes(Arc::new(state));

        assert!(
            !request_with_access_control_request_headers("unauthorized_header".to_string())
                .reply(&routes)
                .await
                .status()
                .is_success()
        );

        assert!(request_with_access_control_request_headers(format!(
            "{MITHRIL_API_VERSION_HEADER},{MITHRIL_ORIGIN_TAG_HEADER}"
        ))
        .reply(&routes)
        .await
        .status()
        .is_success());
    }
}
