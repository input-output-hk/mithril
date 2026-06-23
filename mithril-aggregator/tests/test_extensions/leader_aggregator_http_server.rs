use std::sync::Arc;

use anyhow::Context;
use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use axum_test::TestServer;
use reqwest::Url;

use mithril_aggregator::services::MessageService;
use mithril_common::{
    StdError, StdResult,
    entities::{Epoch, SignedEntityTypeDiscriminants},
    logging::LoggerExtensions,
    messages::SignedEntityTypeDiscriminantsMessage,
    test::messages_extensions::SignedEntityTypeDiscriminantsMessageTestExtension,
};

use crate::test_extensions::RuntimeTester;

pub struct LeaderAggregatorHttpServer {
    server: TestServer,
    url: Url,
}

impl LeaderAggregatorHttpServer {
    pub fn builder() -> LeaderAggregatorHttpServerBuilder {
        LeaderAggregatorHttpServerBuilder::default()
    }

    pub fn spawn(runtime_tester: &RuntimeTester) -> StdResult<Self> {
        Self::builder()
            .with_message_service(runtime_tester.dependencies.message_service.clone())
            .spawn()
    }

    pub fn url(&self) -> &Url {
        &self.url
    }
}

#[derive(Clone)]
struct LeaderAggregatorRoutesState {
    message_service: Arc<dyn MessageService>,
    protocol_configuration_route_settings: ProtocolConfigurationRouteSettings,
    logger: slog::Logger,
}

#[derive(Debug, Clone, Default)]
pub struct ProtocolConfigurationRouteSettings {
    send_unknown_signed_entities: bool,
    send_discontinued_signed_entities: bool,
}

#[derive(Clone, Default)]
pub struct LeaderAggregatorHttpServerBuilder {
    message_service: Option<Arc<dyn MessageService>>,
    protocol_configuration_route_settings: ProtocolConfigurationRouteSettings,
}

impl LeaderAggregatorHttpServerBuilder {
    pub fn with_message_service(mut self, message_service: Arc<dyn MessageService>) -> Self {
        self.message_service = Some(message_service);
        self
    }

    pub fn with_unknown_signed_entities_in_protocol_configuration(mut self) -> Self {
        self.protocol_configuration_route_settings
            .send_unknown_signed_entities = true;
        self
    }

    pub fn with_discontinued_signed_entities_in_protocol_configuration(mut self) -> Self {
        self.protocol_configuration_route_settings
            .send_discontinued_signed_entities = true;
        self
    }

    pub fn spawn(self) -> StdResult<LeaderAggregatorHttpServer> {
        let state = LeaderAggregatorRoutesState {
            message_service: self.message_service.with_context(|| "Message service is required")?,
            protocol_configuration_route_settings: self.protocol_configuration_route_settings,
            logger: slog_scope::logger().new_with_component_name::<LeaderAggregatorHttpServer>(),
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

        let server = TestServer::builder().http_transport().build(router);
        let url = server.server_address().unwrap();

        Ok(LeaderAggregatorHttpServer { server, url })
    }
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
        Ok(Some(mut message)) => {
            if state
                .protocol_configuration_route_settings
                .send_unknown_signed_entities
            {
                message
                    .available_signed_entity_types
                    .insert(SignedEntityTypeDiscriminantsMessage::Unknown);
            }

            if state
                .protocol_configuration_route_settings
                .send_discontinued_signed_entities
            {
                message
                    .available_signed_entity_types
                    .append(&mut SignedEntityTypeDiscriminantsMessage::all_discontinued());
            }

            slog::debug!(
                state.logger, "/protocol-configuration/{epoch}";
                "json" => &serde_json::to_string(&message).unwrap()
            );
            (StatusCode::OK, Json(message)).into_response()
        }
        Ok(None) => StatusCode::NOT_FOUND.into_response(),
        Err(err) => internal_server_error(err).into_response(),
    }
}
