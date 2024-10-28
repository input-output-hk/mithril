use std::collections::BTreeSet;
use warp::Filter;

use mithril_common::{
    entities::SignedEntityTypeDiscriminants,
    messages::{EpochSettingsMessage, SignerMessagePart},
    StdResult,
};

use crate::dependency_injection::EpochServiceWrapper;
use crate::http_server::routes::middlewares;
use crate::DependencyContainer;

pub fn routes(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    epoch_settings(dependency_manager)
}

/// GET /epoch-settings
fn epoch_settings(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("epoch-settings")
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_epoch_service(dependency_manager))
        .and(middlewares::with_allowed_signed_entity_type_discriminants(
            dependency_manager,
        ))
        .and_then(handlers::epoch_settings)
}

async fn get_epoch_settings_message(
    epoch_service: EpochServiceWrapper,
    allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
) -> StdResult<EpochSettingsMessage> {
    let epoch_service = epoch_service.read().await;

    let epoch = epoch_service.epoch_of_current_data()?;
    let protocol_parameters = Some(epoch_service.next_protocol_parameters()?.clone());
    let signer_registration_protocol_parameters = epoch_service
        .signer_registration_protocol_parameters()?
        .clone();
    let next_protocol_parameters = Some(signer_registration_protocol_parameters.clone());
    let current_signers = epoch_service.current_signers()?;
    let next_signers = epoch_service.next_signers()?;

    let cardano_transactions_discriminant =
        allowed_discriminants.get(&SignedEntityTypeDiscriminants::CardanoTransactions);

    let cardano_transactions_signing_config = cardano_transactions_discriminant
        .map(|_| epoch_service.current_cardano_transactions_signing_config())
        .transpose()?
        .cloned();
    let next_cardano_transactions_signing_config = cardano_transactions_discriminant
        .map(|_| epoch_service.next_cardano_transactions_signing_config())
        .transpose()?
        .cloned();

    #[allow(deprecated)]
    let epoch_settings_message = EpochSettingsMessage {
        epoch,
        protocol_parameters,
        next_protocol_parameters,
        signer_registration_protocol_parameters,
        current_signers: SignerMessagePart::from_signers(current_signers.to_vec()),
        next_signers: SignerMessagePart::from_signers(next_signers.to_vec()),
        cardano_transactions_signing_config,
        next_cardano_transactions_signing_config,
    };

    Ok(epoch_settings_message)
}

mod handlers {
    use slog::{warn, Logger};
    use std::collections::BTreeSet;
    use std::convert::Infallible;
    use warp::http::StatusCode;

    use mithril_common::entities::SignedEntityTypeDiscriminants;

    use crate::dependency_injection::EpochServiceWrapper;
    use crate::http_server::routes::epoch_routes::get_epoch_settings_message;
    use crate::http_server::routes::reply;

    /// Epoch Settings
    pub async fn epoch_settings(
        logger: Logger,
        epoch_service: EpochServiceWrapper,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> Result<impl warp::Reply, Infallible> {
        let epoch_settings_message =
            get_epoch_settings_message(epoch_service, allowed_discriminants).await;

        match epoch_settings_message {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"epoch_settings::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value::Null;
    use std::collections::BTreeSet;
    use std::sync::Arc;
    use tokio::sync::RwLock;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        entities::{
            BlockNumber, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters,
            SignedEntityTypeDiscriminants,
        },
        test_utils::{apispec::APISpec, fake_data, MithrilFixtureBuilder},
    };

    use crate::services::FakeEpochService;
    use crate::{entities::AggregatorEpochSettings, http_server::SERVER_BASE_PATH};
    use crate::{initialize_dependencies, services::FakeEpochServiceBuilder};

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
            .and(routes(&dependency_manager).with(cors))
    }

    #[tokio::test]
    async fn get_epoch_settings_message_with_cardano_transactions_enabled() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
        let epoch_service = Arc::new(RwLock::new(epoch_service));
        let allowed_discriminants =
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]);

        let message = get_epoch_settings_message(epoch_service, allowed_discriminants)
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

        let allowed_discriminants = BTreeSet::new();
        let message = get_epoch_settings_message(epoch_service, allowed_discriminants)
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
        let signer_registration_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(103, 30, 0.5),
            ..AggregatorEpochSettings::dummy()
        };

        let epoch_service = FakeEpochServiceBuilder {
            current_epoch_settings,
            next_epoch_settings: next_epoch_settings.clone(),
            signer_registration_epoch_settings: signer_registration_epoch_settings.clone(),
            current_signers_with_stake: fake_data::signers_with_stakes(5),
            next_signers_with_stake: fake_data::signers_with_stakes(3),
            ..FakeEpochServiceBuilder::dummy(Epoch(1))
        }
        .build();

        let message = get_epoch_settings_message(
            Arc::new(RwLock::new(epoch_service)),
            SignedEntityTypeDiscriminants::all(),
        )
        .await
        .unwrap();

        #[allow(deprecated)]
        let message_protocol_parameters = message.protocol_parameters.unwrap();
        assert_eq!(
            message_protocol_parameters,
            next_epoch_settings.protocol_parameters
        );

        #[allow(deprecated)]
        let message_next_protocol_parameters = message.next_protocol_parameters.unwrap();
        assert_eq!(
            message_next_protocol_parameters,
            signer_registration_epoch_settings.protocol_parameters
        );

        assert_eq!(
            message.signer_registration_protocol_parameters,
            signer_registration_epoch_settings.protocol_parameters
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

        let epoch_service = FakeEpochServiceBuilder {
            current_epoch_settings: current_epoch_settings.clone(),
            next_epoch_settings: next_epoch_settings.clone(),
            signer_registration_epoch_settings: AggregatorEpochSettings::dummy(),
            current_signers_with_stake: fake_data::signers_with_stakes(5),
            next_signers_with_stake: fake_data::signers_with_stakes(3),
            ..FakeEpochServiceBuilder::dummy(Epoch(1))
        }
        .build();

        let message = get_epoch_settings_message(
            Arc::new(RwLock::new(epoch_service)),
            SignedEntityTypeDiscriminants::all(),
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
