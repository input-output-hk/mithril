use crate::http_server::routes::{
    artifact_routes, certificate_routes, epoch_routes, http_server_child_logger, root_routes,
    signatures_routes, signer_routes, statistics_routes,
};
use crate::http_server::SERVER_BASE_PATH;
use crate::DependencyContainer;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::entities::{CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants};
use mithril_common::{CardanoNetwork, MITHRIL_API_VERSION_HEADER};

use slog::{warn, Logger};
use std::collections::BTreeSet;
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
    pub server_url: String,
    pub allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    pub cardano_transactions_prover_max_hashes_allowed_by_request: usize,
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
    pub snapshot_directory: PathBuf,
}

#[cfg(test)]
impl RouterConfig {
    pub fn dummy() -> Self {
        Self {
            network: CardanoNetwork::DevNet(87),
            server_url: "http://0.0.0.0:8000/".to_string(),
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ]),
            cardano_transactions_prover_max_hashes_allowed_by_request: 1_000,
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
            snapshot_directory: PathBuf::from("/dummy/snapshot/directory"),
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
}

/// Routes
pub fn routes(
    state: Arc<RouterState>,
) -> impl Filter<Extract = (impl Reply,), Error = Rejection> + Clone {
    let cors = warp::cors()
        .allow_any_origin()
        .allow_headers(vec!["content-type", MITHRIL_API_VERSION_HEADER])
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
                .or(artifact_routes::mithril_stake_distribution::routes(&state))
                .or(artifact_routes::cardano_stake_distribution::routes(&state))
                .or(artifact_routes::cardano_transaction::routes(&state))
                .or(proof_routes::routes(&state))
                .or(signer_routes::routes(&state))
                .or(signatures_routes::routes(&state))
                .or(epoch_routes::routes(&state))
                .or(statistics_routes::routes(&state))
                .or(root_routes::routes(&state))
                .with(cors),
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
    } else {
        Err(reject)
    }
}

#[cfg(test)]
mod tests {
    use semver::Version;
    use std::collections::HashMap;

    use mithril_common::{
        entities::Epoch,
        era::{EraChecker, SupportedEra},
    };

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
}
