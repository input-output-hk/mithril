use super::middlewares;
use crate::http_server::routes::router::RouterState;
use warp::Filter;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    root(router_state)
}

/// GET /
fn root(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path::end()
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_api_version_provider(router_state))
        .and(middlewares::with_allowed_signed_entity_type_discriminants(
            router_state,
        ))
        .and(middlewares::with_config(router_state))
        .and_then(handlers::root)
}

mod handlers {
    use std::collections::BTreeSet;
    use std::{convert::Infallible, sync::Arc};

    use slog::Logger;
    use warp::http::StatusCode;

    use mithril_common::api_version::APIVersionProvider;
    use mithril_common::entities::SignedEntityTypeDiscriminants;
    use mithril_common::messages::{
        AggregatorCapabilities, AggregatorFeaturesMessage, CardanoTransactionsProverCapabilities,
    };

    use crate::http_server::routes::reply::json;
    use crate::{unwrap_to_internal_server_error, Configuration};

    /// Root
    pub async fn root(
        logger: Logger,
        api_version_provider: Arc<APIVersionProvider>,
        allowed_signed_entity_type_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
        configuration: Configuration,
    ) -> Result<impl warp::Reply, Infallible> {
        let open_api_version = unwrap_to_internal_server_error!(
            api_version_provider.compute_current_version(),
            logger => "root::error"
        );

        let mut capabilities = AggregatorCapabilities {
            signed_entity_types: allowed_signed_entity_type_discriminants,
            cardano_transactions_prover: None,
            cardano_transactions_signing_config: None,
        };

        if capabilities
            .signed_entity_types
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions)
        {
            capabilities.cardano_transactions_prover =
                Some(CardanoTransactionsProverCapabilities {
                    max_hashes_allowed_by_request: configuration
                        .cardano_transactions_prover_max_hashes_allowed_by_request,
                });

            capabilities.cardano_transactions_signing_config =
                Some(configuration.cardano_transactions_signing_config.clone());
        }

        Ok(json(
            &AggregatorFeaturesMessage {
                open_api_version: open_api_version.to_string(),
                documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
                capabilities,
            },
            StatusCode::OK,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::dependency_injection::DependenciesBuilder;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::Configuration;
    use mithril_common::entities::{
        BlockNumber, CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants,
    };
    use mithril_common::messages::{
        AggregatorCapabilities, AggregatorFeaturesMessage, CardanoTransactionsProverCapabilities,
    };
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
        state: RouterState,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any()
            .and(warp::path(SERVER_BASE_PATH))
            .and(routes(&state).with(cors))
    }

    #[tokio::test]
    async fn test_root_route_ok() {
        let method = Method::GET.as_str();
        let path = "/";
        let config = Configuration {
            signed_entity_types: Some(format!(
                "{}, {}, {}",
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            )),
            ..Configuration::new_sample()
        };
        let mut builder = DependenciesBuilder::new_with_stdout_logger(config);
        let dependency_manager = builder.build_dependency_container().await.unwrap();

        let expected_open_api_version = dependency_manager
            .api_version_provider
            .clone()
            .compute_current_version()
            .unwrap()
            .to_string();

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        let response_body: AggregatorFeaturesMessage =
            serde_json::from_slice(response.body()).unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response_body,
            AggregatorFeaturesMessage {
                open_api_version: expected_open_api_version,
                documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
                capabilities: AggregatorCapabilities {
                    signed_entity_types: BTreeSet::from_iter([
                        SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                        SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    ]),
                    cardano_transactions_prover: None,
                    cardano_transactions_signing_config: None,
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
    async fn test_root_route_ok_with_cardano_transactions_enabled() {
        let method = Method::GET.as_str();
        let path = "/";
        let config = Configuration {
            signed_entity_types: Some(format!(
                "{}",
                SignedEntityTypeDiscriminants::CardanoTransactions
            )),
            ..Configuration::new_sample()
        };
        let mut builder = DependenciesBuilder::new_with_stdout_logger(config);
        let mut dependency_manager = builder.build_dependency_container().await.unwrap();
        dependency_manager
            .config
            .cardano_transactions_prover_max_hashes_allowed_by_request = 99;
        let signing_config = CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(70),
            step: BlockNumber(15),
        };
        dependency_manager
            .config
            .cardano_transactions_signing_config = signing_config.clone();

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        let response_body: AggregatorFeaturesMessage =
            serde_json::from_slice(response.body()).unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response_body.capabilities.cardano_transactions_prover,
            Some(CardanoTransactionsProverCapabilities {
                max_hashes_allowed_by_request: 99
            })
        );
        assert_eq!(
            response_body
                .capabilities
                .cardano_transactions_signing_config,
            Some(signing_config)
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
