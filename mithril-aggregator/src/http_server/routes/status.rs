use warp::Filter;

use mithril_common::{messages::AggregatorStatusMessage, StdResult};

use crate::{
    dependency_injection::EpochServiceWrapper,
    http_server::routes::{middlewares, router::RouterState},
};

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    status(router_state)
}

/// GET /status
fn status(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("status")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_epoch_service(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.cardano_node_version.clone()
        }))
        .and_then(handlers::status)
}

async fn get_aggregator_status_message(
    epoch_service: EpochServiceWrapper,
    cardano_node_version: String,
) -> StdResult<AggregatorStatusMessage> {
    let epoch_service = epoch_service.read().await;

    let epoch = epoch_service.epoch_of_current_data()?;
    let cardano_era = epoch_service.cardano_era()?;
    let mithril_era = epoch_service.mithril_era()?;
    let aggregator_node_version = env!("CARGO_PKG_VERSION").to_string();
    let protocol_parameters = epoch_service.current_protocol_parameters()?.clone();
    let next_protocol_parameters = epoch_service.next_protocol_parameters()?.clone();
    let total_signers = epoch_service.current_signers()?.len();
    let total_next_signers = epoch_service.next_signers()?.len();
    let total_stakes_signers = epoch_service.total_stakes_signers()?;
    let total_next_stakes_signers = epoch_service.total_next_stakes_signers()?;
    let total_cardano_spo = epoch_service.total_spo()?;
    let total_cardano_stake = epoch_service.total_stake()?;

    let message = AggregatorStatusMessage {
        epoch,
        cardano_era,
        mithril_era,
        cardano_node_version,
        aggregator_node_version,
        protocol_parameters,
        next_protocol_parameters,
        total_signers,
        total_next_signers,
        total_stakes_signers,
        total_next_stakes_signers,
        total_cardano_spo,
        total_cardano_stake,
    };

    Ok(message)
}

mod handlers {
    use std::convert::Infallible;

    use slog::{warn, Logger};
    use warp::http::StatusCode;

    use crate::{
        dependency_injection::EpochServiceWrapper,
        http_server::routes::{reply, status::get_aggregator_status_message},
    };

    /// Status
    pub async fn status(
        logger: Logger,
        epoch_service: EpochServiceWrapper,
        cardano_node_version: String,
    ) -> Result<impl warp::Reply, Infallible> {
        let aggregator_status_message =
            get_aggregator_status_message(epoch_service, cardano_node_version).await;

        match aggregator_status_message {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"aggregator_status::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        entities::{Epoch, ProtocolParameters, Stake},
        test_utils::{apispec::APISpec, fake_data, MithrilFixtureBuilder},
    };

    use crate::{
        entities::AggregatorEpochSettings,
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        services::{FakeEpochService, FakeEpochServiceBuilder},
    };

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
    async fn status_route_ko_500() {
        let dependency_manager = initialize_dependencies().await;
        let method = Method::GET.as_str();
        let path = "/status";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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

    #[tokio::test]
    async fn status_route_ok_200() {
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));

        let method = Method::GET.as_str();
        let path = "/status";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
    async fn retrieves_correct_protocol_parameters_from_epoch_service() {
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
            current_epoch_settings: current_epoch_settings.clone(),
            next_epoch_settings: next_epoch_settings.clone(),
            signer_registration_epoch_settings,
            ..FakeEpochServiceBuilder::dummy(Epoch(3))
        }
        .build();

        let message =
            get_aggregator_status_message(Arc::new(RwLock::new(epoch_service)), String::new())
                .await
                .unwrap();

        assert_eq!(
            message.protocol_parameters,
            current_epoch_settings.protocol_parameters
        );

        assert_eq!(
            message.next_protocol_parameters,
            next_epoch_settings.protocol_parameters
        );
    }

    #[tokio::test]
    async fn retrieves_correct_total_signers_from_epoch_service() {
        let total_signers = 12;
        let total_next_signers = 345;
        let epoch_service = FakeEpochServiceBuilder {
            current_signers_with_stake: fake_data::signers_with_stakes(total_signers),
            next_signers_with_stake: fake_data::signers_with_stakes(total_next_signers),
            ..FakeEpochServiceBuilder::dummy(Epoch(3))
        }
        .build();
        let epoch_service = Arc::new(RwLock::new(epoch_service));

        let message = get_aggregator_status_message(epoch_service.clone(), String::new())
            .await
            .unwrap();

        assert_eq!(message.total_signers, total_signers);
        assert_eq!(message.total_next_signers, total_next_signers);
    }

    #[tokio::test]
    async fn retrieves_correct_total_stakes_from_epoch_service() {
        let current_signers_with_stake = fake_data::signers_with_stakes(4);
        let next_signers_with_stake = fake_data::signers_with_stakes(7);
        let total_stakes_signers: Stake = current_signers_with_stake.iter().map(|s| s.stake).sum();
        let total_next_stakes_signers: Stake =
            next_signers_with_stake.iter().map(|s| s.stake).sum();

        assert_ne!(total_stakes_signers, total_next_stakes_signers);

        let epoch_service = FakeEpochServiceBuilder {
            current_signers_with_stake,
            next_signers_with_stake,
            ..FakeEpochServiceBuilder::dummy(Epoch(3))
        }
        .build();
        let epoch_service = Arc::new(RwLock::new(epoch_service));

        let message = get_aggregator_status_message(epoch_service.clone(), String::new())
            .await
            .unwrap();

        assert_eq!(message.total_stakes_signers, total_stakes_signers);
        assert_eq!(message.total_next_stakes_signers, total_next_stakes_signers);
    }
}