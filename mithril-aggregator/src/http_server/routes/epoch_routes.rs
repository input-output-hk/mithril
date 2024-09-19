use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    epoch_settings(dependency_manager)
}

/// GET /epoch-settings
fn epoch_settings(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("epoch-settings")
        .and(warp::get())
        .and(middlewares::with_epoch_service(dependency_manager.clone()))
        .and(middlewares::with_signed_entity_config(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_config(dependency_manager))
        .and_then(handlers::epoch_settings)
}

mod handlers {
    use std::convert::Infallible;

    use slog_scope::{debug, warn};
    use warp::http::StatusCode;

    use mithril_common::{
        entities::{SignedEntityConfig, SignedEntityTypeDiscriminants},
        messages::{EpochSettingsMessage, SignerMessagePart},
    };

    use crate::http_server::routes::reply;
    use crate::{dependency_injection::EpochServiceWrapper, Configuration};

    /// Epoch Settings
    pub async fn epoch_settings(
        epoch_service: EpochServiceWrapper,
        signed_entity_config: SignedEntityConfig,
        configuration: Configuration,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: epoch_settings");
        let epoch_service = epoch_service.read().await;

        let cardano_transactions_signing_config = signed_entity_config
            .list_allowed_signed_entity_types_discriminants()
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions)
            .then_some(configuration.cardano_transactions_signing_config);

        match (
            epoch_service.epoch_of_current_data(),
            epoch_service.next_protocol_parameters(),
            epoch_service.upcoming_protocol_parameters(),
            epoch_service.current_signers(),
            epoch_service.next_signers(),
        ) {
            (
                Ok(epoch),
                Ok(protocol_parameters),
                Ok(next_protocol_parameters),
                Ok(current_signers),
                Ok(next_signers),
            ) => {
                let epoch_settings_message = EpochSettingsMessage {
                    epoch,
                    protocol_parameters: protocol_parameters.clone(),
                    next_protocol_parameters: next_protocol_parameters.clone(),
                    current_signers: SignerMessagePart::from_signers(current_signers.to_vec()),
                    next_signers: SignerMessagePart::from_signers(next_signers.to_vec()),
                    cardano_transactions_signing_config: cardano_transactions_signing_config
                        .clone(),
                    next_cardano_transactions_signing_config: cardano_transactions_signing_config,
                };
                Ok(reply::json(&epoch_settings_message, StatusCode::OK))
            }
            (Err(err), _, _, _, _)
            | (_, Err(err), _, _, _)
            | (_, _, Err(err), _, _)
            | (_, _, _, Err(err), _)
            | (_, _, _, _, Err(err)) => {
                warn!("epoch_settings::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::http::{Method, StatusCode};
    use warp::test::request;

    use mithril_common::{
        entities::{
            BlockNumber, CardanoTransactionsSigningConfig, Epoch, SignedEntityTypeDiscriminants,
        },
        messages::EpochSettingsMessage,
        test_utils::{apispec::APISpec, MithrilFixtureBuilder},
    };

    use crate::http_server::SERVER_BASE_PATH;
    use crate::initialize_dependencies;
    use crate::services::FakeEpochService;

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
    async fn test_epoch_settings_get_ok() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

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
    async fn test_epoch_settings_get_ok_with_cardano_transactions_enabled() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));
        dependency_manager
            .signed_entity_config
            .allowed_discriminants =
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]);
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
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        let response_body: EpochSettingsMessage = serde_json::from_slice(response.body()).unwrap();

        assert_eq!(
            response_body.current_cardano_transactions_signing_config,
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

    #[tokio::test]
    async fn test_epoch_settings_get_ok_with_cardano_transactions_not_enabled() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));
        dependency_manager
            .signed_entity_config
            .allowed_discriminants = BTreeSet::new();
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
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        let response_body: EpochSettingsMessage = serde_json::from_slice(response.body()).unwrap();

        assert_eq!(
            response_body.current_cardano_transactions_signing_config,
            None
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
    async fn test_epoch_settings_get_ko_500() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let dependency_manager = initialize_dependencies().await;

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }
}
