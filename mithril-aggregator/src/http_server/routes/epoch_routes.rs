use std::sync::Arc;
use warp::Filter;

use mithril_common::{
    entities::{SignedEntityConfig, SignedEntityTypeDiscriminants},
    messages::{EpochSettingsMessage, SignerMessagePart},
    StdResult,
};

use crate::dependency_injection::EpochServiceWrapper;
use crate::http_server::routes::middlewares;
use crate::DependencyContainer;

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
        .and(middlewares::with_signed_entity_config(dependency_manager))
        .and_then(handlers::epoch_settings)
}

async fn get_epoch_settings_message(
    epoch_service: EpochServiceWrapper,
    signed_entity_config: SignedEntityConfig,
) -> StdResult<EpochSettingsMessage> {
    let epoch_service = epoch_service.read().await;

    let epoch = epoch_service.epoch_of_current_data()?;
    let protocol_parameters = epoch_service.next_protocol_parameters()?.clone();
    let next_protocol_parameters = epoch_service.upcoming_protocol_parameters()?.clone();
    let current_signers = epoch_service.current_signers()?;
    let next_signers = epoch_service.next_signers()?;

    let allowed_types = signed_entity_config.list_allowed_signed_entity_types_discriminants();
    let cardano_transactions_discriminant =
        allowed_types.get(&SignedEntityTypeDiscriminants::CardanoTransactions);

    let cardano_transactions_signing_config = cardano_transactions_discriminant
        .map(|_| epoch_service.current_cardano_transactions_signing_config())
        .transpose()?
        .cloned();
    let next_cardano_transactions_signing_config = cardano_transactions_discriminant
        .map(|_| epoch_service.next_cardano_transactions_signing_config())
        .transpose()?
        .cloned();

    let epoch_settings_message = EpochSettingsMessage {
        epoch,
        protocol_parameters,
        next_protocol_parameters,
        current_signers: SignerMessagePart::from_signers(current_signers.to_vec()),
        next_signers: SignerMessagePart::from_signers(next_signers.to_vec()),
        cardano_transactions_signing_config,
        next_cardano_transactions_signing_config,
    };

    Ok(epoch_settings_message)
}

mod handlers {
    use slog_scope::debug;
    use std::convert::Infallible;
    use warp::http::StatusCode;

    use mithril_common::entities::SignedEntityConfig;

    use crate::dependency_injection::EpochServiceWrapper;
    use crate::http_server::routes::epoch_routes::get_epoch_settings_message;
    use crate::http_server::routes::reply;

    /// Epoch Settings
    pub async fn epoch_settings(
        epoch_service: EpochServiceWrapper,
        signed_entity_config: SignedEntityConfig,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: epoch_settings");
        let epoch_settings_message =
            get_epoch_settings_message(epoch_service, signed_entity_config).await;

        match epoch_settings_message {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => Ok(reply::server_error(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value::Null;
    use std::collections::BTreeSet;
    use tokio::sync::RwLock;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        entities::{
            BlockNumber, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters,
            SignedEntityConfig, SignedEntityTypeDiscriminants,
        },
        test_utils::{apispec::APISpec, fake_data, MithrilFixtureBuilder},
    };

    use crate::initialize_dependencies;
    use crate::services::FakeEpochService;
    use crate::{entities::AggregatorEpochSettings, http_server::SERVER_BASE_PATH};

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
    async fn get_epoch_settings_message_with_cardano_transactions_enabled() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
        let epoch_service = Arc::new(RwLock::new(epoch_service));

        let cardano_transactions_signing_config = CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(70),
            step: BlockNumber(15),
        };
        let signed_entity_config = SignedEntityConfig {
            cardano_transactions_signing_config: cardano_transactions_signing_config.clone(),
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]),
            ..SignedEntityConfig::dummy()
        };

        let message = get_epoch_settings_message(epoch_service, signed_entity_config)
            .await
            .unwrap();

        assert!(message.cardano_transactions_signing_config.is_some());
        assert!(message.next_cardano_transactions_signing_config.is_some(),);
    }

    #[tokio::test]
    async fn get_epoch_settings_message_with_cardano_transactions_not_enabled() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
        let epoch_service = Arc::new(RwLock::new(epoch_service));

        let cardano_transactions_signing_config = CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(70),
            step: BlockNumber(15),
        };
        let signed_entity_config = SignedEntityConfig {
            cardano_transactions_signing_config,
            allowed_discriminants: BTreeSet::new(),
            ..SignedEntityConfig::dummy()
        };

        let message = get_epoch_settings_message(epoch_service, signed_entity_config)
            .await
            .unwrap();

        assert_eq!(message.cardano_transactions_signing_config, None);
        assert_eq!(message.next_cardano_transactions_signing_config, None);
    }

    #[tokio::test]
    async fn get_epoch_settings_message_retrieves_protocol_parameters_from_epoch_service() {
        let current_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(101, 10, 0.5),
            ..AggregatorEpochSettings::dummy()
        };
        let next_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(102, 20, 0.5),
            ..AggregatorEpochSettings::dummy()
        };
        let upcoming_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(103, 30, 0.5),
            ..AggregatorEpochSettings::dummy()
        };

        let epoch_service = FakeEpochService::with_data(
            Epoch(1),
            &current_epoch_settings,
            &next_epoch_settings,
            &upcoming_epoch_settings,
            &fake_data::signers_with_stakes(5),
            &fake_data::signers_with_stakes(3),
            SignedEntityConfig::dummy(),
        );

        let message = get_epoch_settings_message(
            Arc::new(RwLock::new(epoch_service)),
            SignedEntityConfig::dummy(),
        )
        .await
        .unwrap();

        assert_eq!(
            message.protocol_parameters,
            next_epoch_settings.protocol_parameters
        );
        assert_eq!(
            message.next_protocol_parameters,
            upcoming_epoch_settings.protocol_parameters
        );
    }

    #[tokio::test]
    async fn get_epoch_settings_message_retrieves_signing_configuration_from_epoch_service() {
        let current_epoch_settings = AggregatorEpochSettings {
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::new(
                BlockNumber(100),
                BlockNumber(15),
            ),
            ..AggregatorEpochSettings::dummy()
        };
        let next_epoch_settings = AggregatorEpochSettings {
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::new(
                BlockNumber(200),
                BlockNumber(15),
            ),
            ..AggregatorEpochSettings::dummy()
        };

        let epoch_service = FakeEpochService::with_data(
            Epoch(1),
            &current_epoch_settings,
            &next_epoch_settings,
            &AggregatorEpochSettings::dummy(),
            &fake_data::signers_with_stakes(5),
            &fake_data::signers_with_stakes(3),
            SignedEntityConfig::dummy(),
        );

        let message = get_epoch_settings_message(
            Arc::new(RwLock::new(epoch_service)),
            SignedEntityConfig::dummy(),
        )
        .await
        .unwrap();

        assert_eq!(
            message.cardano_transactions_signing_config,
            Some(current_epoch_settings.cardano_transactions_signing_config),
        );
        assert_eq!(
            message.next_cardano_transactions_signing_config,
            Some(next_epoch_settings.cardano_transactions_signing_config),
        );
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
