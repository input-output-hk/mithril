use crate::DependencyContainer;
use mithril_common::entities::SignedEntityTypeDiscriminants;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeSet, sync::Arc};
use warp::Filter;

use super::middlewares;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct RootRouteMessage {
    pub open_api_version: String,
    pub documentation_url: String,
    pub capabilities: AggregatorCapabilities,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct AggregatorCapabilities {
    pub signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_prover: Option<CardanoTransactionsProverCapabilities>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct CardanoTransactionsProverCapabilities {
    max_hashes_allowed_by_request: usize,
}

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    root(dependency_manager)
}

/// GET /
fn root(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path::end()
        .and(middlewares::with_api_version_provider(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_signed_entity_config(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_config(dependency_manager))
        .and_then(handlers::root)
}

mod handlers {
    use std::{convert::Infallible, sync::Arc};

    use slog_scope::{debug, warn};
    use warp::http::StatusCode;

    use mithril_common::api_version::APIVersionProvider;
    use mithril_common::entities::{SignedEntityConfig, SignedEntityTypeDiscriminants};

    use crate::http_server::routes::reply::json;
    use crate::http_server::routes::root_routes::{
        AggregatorCapabilities, CardanoTransactionsProverCapabilities, RootRouteMessage,
    };
    use crate::{unwrap_to_internal_server_error, Configuration};

    /// Root
    pub async fn root(
        api_version_provider: Arc<APIVersionProvider>,
        signed_entity_config: SignedEntityConfig,
        configuration: Configuration,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: root");

        let open_api_version = unwrap_to_internal_server_error!(
            api_version_provider.compute_current_version(),
            "root::error"
        );

        let signed_entity_types =
            signed_entity_config.list_allowed_signed_entity_types_discriminants();

        let cardano_transactions_prover_capabilities = signed_entity_types
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions)
            .then_some(CardanoTransactionsProverCapabilities {
                max_hashes_allowed_by_request: configuration
                    .cardano_transactions_prover_max_hashes_allowed_by_request,
            });

        Ok(json(
            &RootRouteMessage {
                open_api_version: open_api_version.to_string(),
                documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
                capabilities: AggregatorCapabilities {
                    signed_entity_types,
                    cardano_transactions_prover: cardano_transactions_prover_capabilities,
                },
            },
            StatusCode::OK,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use crate::{initialize_dependencies, DependencyContainer};
    use mithril_common::test_utils::apispec::APISpec;
    use serde_json::Value::Null;
    use std::collections::BTreeSet;
    use std::sync::Arc;
    use warp::http::Method;
    use warp::http::StatusCode;
    use warp::test::request;
    use warp::Filter;

    use super::*;

    fn setup_router(
        dependency_manager: Arc<DependencyContainer>,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any()
            .and(warp::path(SERVER_BASE_PATH))
            .and(routes(dependency_manager).with(cors))
    }

    #[tokio::test]
    async fn test_root_route_ok() {
        let method = Method::GET.as_str();
        let path = "/";
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager
            .signed_entity_config
            .allowed_discriminants = BTreeSet::from([
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
        ]);
        let expected_open_api_version = dependency_manager
            .api_version_provider
            .clone()
            .compute_current_version()
            .unwrap()
            .to_string();

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        let response_body: RootRouteMessage = serde_json::from_slice(response.body()).unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response_body,
            RootRouteMessage {
                open_api_version: expected_open_api_version,
                documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
                capabilities: AggregatorCapabilities {
                    signed_entity_types: BTreeSet::from_iter([
                        SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                        SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    ]),
                    cardano_transactions_prover: None
                },
            }
        );

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_root_route_ok_with_cardano_transactions_prover_capabilities() {
        let method = Method::GET.as_str();
        let path = "/";
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager
            .signed_entity_config
            .allowed_discriminants =
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]);
        dependency_manager
            .config
            .cardano_transactions_prover_max_hashes_allowed_by_request = 99;

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        let response_body: RootRouteMessage = serde_json::from_slice(response.body()).unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response_body.capabilities.cardano_transactions_prover,
            Some(CardanoTransactionsProverCapabilities {
                max_hashes_allowed_by_request: 99
            })
        );

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }
}
