use std::sync::Arc;

use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use axum_test::TestServer;
use mithril_aggregator::services::MessageService;
use mithril_common::entities::{Epoch, SignedEntityTypeDiscriminants};
use mithril_common::logging::LoggerExtensions;
use mithril_common::{StdError, StdResult};
use reqwest::Url;

use crate::test_extensions::RuntimeTester;

pub struct LeaderAggregatorHttpServer {
    server: TestServer,
    url: Url,
}

impl LeaderAggregatorHttpServer {
    pub fn spawn(runtime_tester: &RuntimeTester) -> StdResult<Self> {
        let state = LeaderAggregatorRoutesState {
            message_service: runtime_tester.dependencies.message_service.clone(),
            logger: slog_scope::logger().new_with_component_name::<Self>(),
        };
        let router = Router::new()
            .route("/epoch-settings", get(epoch_settings))
            .route("/certificates", get(certificates_list))
            .route("/certificate/genesis", get(certificate_last_genesis))
            .route("/certificate/{hash}", get(certificate_by_hash))
            .route(
                "/protocol-configuration/{epoch}",
                get(protocol_configuration_by_epoch),
            )
            .with_state(state);

        let server = TestServer::builder().http_transport().build(router)?;
        let url = server.server_address().unwrap();

        Ok(Self { server, url })
    }

    pub fn url(&self) -> &Url {
        &self.url
    }
}

#[derive(Clone)]
struct LeaderAggregatorRoutesState {
    message_service: Arc<dyn MessageService>,
    logger: slog::Logger,
}

fn internal_server_error(err: StdError) -> impl IntoResponse {
    (StatusCode::INTERNAL_SERVER_ERROR, Json(err.to_string()))
}

async fn epoch_settings(state: State<LeaderAggregatorRoutesState>) -> Response {
    slog::debug!(state.logger, "/epoch-settings");
    let allowed_discriminants = SignedEntityTypeDiscriminants::all();
    let epoch_settings_message = state
        .message_service
        .get_epoch_settings_message(allowed_discriminants)
        .await;

    match epoch_settings_message {
        Ok(message) => (StatusCode::OK, Json(message)).into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}

async fn certificates_list(state: State<LeaderAggregatorRoutesState>) -> Response {
    slog::debug!(state.logger, "/certificates");
    match state.message_service.get_certificate_list_message(5).await {
        Ok(message) => (StatusCode::OK, Json(message)).into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}

async fn certificate_last_genesis(state: State<LeaderAggregatorRoutesState>) -> Response {
    slog::debug!(state.logger, "/certificate/genesis");
    match state.message_service.get_latest_genesis_certificate_message().await {
        Ok(Some(message)) => (StatusCode::OK, Json(message)).into_response(),
        Ok(None) => StatusCode::NOT_FOUND.into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}

async fn certificate_by_hash(
    Path(hash): Path<String>,
    state: State<LeaderAggregatorRoutesState>,
) -> Response {
    slog::debug!(state.logger, "/certificate/{hash}");
    match state.message_service.get_certificate_message(&hash).await {
        Ok(Some(message)) => (StatusCode::OK, Json(message)).into_response(),
        Ok(None) => StatusCode::NOT_FOUND.into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}

async fn protocol_configuration_by_epoch(
    Path(epoch): Path<u64>,
    state: State<LeaderAggregatorRoutesState>,
) -> Response {
    slog::debug!(state.logger, "/protocol-configuration/{epoch}");
    match state
        .message_service
        .get_protocol_configuration_message(Epoch(epoch), SignedEntityTypeDiscriminants::all())
        .await
    {
        Ok(Some(message)) => (StatusCode::OK, Json(message)).into_response(),
        Ok(None) => StatusCode::NOT_FOUND.into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}
