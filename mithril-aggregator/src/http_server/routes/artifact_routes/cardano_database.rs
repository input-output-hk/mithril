use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;
use warp::Filter;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_database_list(router_state)
        .or(artifact_cardano_database_digest_list(router_state))
        .or(artifact_cardano_database_by_id(router_state))
        .or(serve_cardano_database_dir(router_state))
}

/// GET /artifact/cardano-database
fn artifact_cardano_database_list(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_http_message_service(router_state))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/cardano-database/:id
fn artifact_cardano_database_by_id(
    dependency_manager: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database" / String)
        .and(warp::get())
        .and(middlewares::with_client_metadata(dependency_manager))
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and(middlewares::with_metrics_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

/// GET /artifact/cardano-database/digests
fn artifact_cardano_database_digest_list(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-database" / "digests")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_http_message_service(router_state))
        .and_then(handlers::list_digests)
}

fn serve_cardano_database_dir(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path(crate::http_server::CARDANO_DATABASE_DOWNLOAD_PATH)
        .and(warp::fs::dir(
            router_state.configuration.cardano_db_artifacts_directory.clone(),
        ))
        .and(middlewares::with_logger(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.allow_http_serve_directory
        }))
        .and_then(handlers::ensure_downloaded_file_is_a_cardano_database_artifact)
}

mod handlers {
    use slog::{debug, warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    use crate::http_server::routes::middlewares::ClientMetadata;
    use crate::http_server::routes::reply;
    use crate::services::MessageService;
    use crate::MetricsService;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List artifacts
    pub async fn list_artifacts(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match http_message_service
            .get_cardano_database_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"list_artifacts_cardano_database"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Get artifact by signed entity id
    pub async fn get_artifact_by_signed_entity_id(
        signed_entity_id: String,
        client_metadata: ClientMetadata,
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_artifact_detail_cardano_database_total_served_since_startup()
            .increment(&[
                client_metadata.origin_tag.as_deref().unwrap_or_default(),
                client_metadata.client_type.as_deref().unwrap_or_default(),
            ]);

        match http_message_service
            .get_cardano_database_message(&signed_entity_id)
            .await
        {
            Ok(Some(signed_entity)) => Ok(reply::json(&signed_entity, StatusCode::OK)),
            Ok(None) => {
                warn!(logger, "cardano_database_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger,"cardano_database_details::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Download a file if it's a Cardano_database artifact file
    // TODO: this function should probable be unit tested once the file naming convention is defined
    pub async fn ensure_downloaded_file_is_a_cardano_database_artifact(
        reply: warp::fs::File,
        logger: Logger,
        allow_http_serve_directory: bool,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            logger,
            ">> ensure_downloaded_file_is_a_cardano_database / file: `{}`",
            filepath.display()
        );

        if !allow_http_serve_directory {
            warn!(logger, "ensure_downloaded_file_is_a_cardano_database::error"; "error" => "http serve directory is disabled");
            return Ok(reply::empty(StatusCode::FORBIDDEN));
        }

        // TODO: enhance this check with a regular expression once the file naming convention is defined
        let file_is_a_cardano_database_archive = filepath.to_string_lossy().contains("ancillary")
            || filepath.to_string_lossy().contains("immutable")
            || filepath.to_string_lossy().contains("digests");
        match file_is_a_cardano_database_archive {
            true => Ok(reply::add_content_disposition_header(reply, &filepath)),
            false => {
                warn!(logger,"ensure_downloaded_file_is_a_cardano_database::error"; "error" => "file is not a Cardano database archive");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
        }
    }

    /// List digests
    pub async fn list_digests(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match http_message_service.get_cardano_database_digest_list_message().await {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"list_digests_cardano_database"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_api_spec::APISpec;
    use mithril_common::messages::{
        CardanoDatabaseDigestListItemMessage, CardanoDatabaseSnapshotListItemMessage,
        CardanoDatabaseSnapshotMessage,
    };
    use mithril_common::{MITHRIL_CLIENT_TYPE_HEADER, MITHRIL_ORIGIN_TAG_HEADER};
    use mithril_persistence::sqlite::HydrationError;

    use crate::{initialize_dependencies, services::MockMessageService};

    use super::*;

    fn setup_router(
        state: RouterState,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(routes(&state).with(cors))
    }

    #[tokio::test]
    async fn test_cardano_database_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_list_message()
            .return_once(|_| Ok(vec![CardanoDatabaseSnapshotListItemMessage::dummy()]))
            .once();

        let mut dependency_manager = initialize_dependencies!().await;

        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
    async fn test_cardano_database_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_list_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
    async fn test_cardano_database_detail_increments_artifact_detail_total_served_since_startup_metric(
    ) {
        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{hash}";
        let dependency_manager = Arc::new(initialize_dependencies!().await);
        let initial_counter_value = dependency_manager
            .metrics_service
            .get_artifact_detail_cardano_database_total_served_since_startup()
            .get(&["TEST", "CLI"]);

        request()
            .method(method)
            .path(path)
            .header(MITHRIL_ORIGIN_TAG_HEADER, "TEST")
            .header(MITHRIL_CLIENT_TYPE_HEADER, "CLI")
            .reply(&setup_router(RouterState::new_with_origin_tag_white_list(
                dependency_manager.clone(),
                &["TEST"],
            )))
            .await;

        assert_eq!(
            initial_counter_value + 1,
            dependency_manager
                .metrics_service
                .get_artifact_detail_cardano_database_total_served_since_startup()
                .get(&["TEST", "CLI"]),
        );
    }

    #[tokio::test]
    async fn test_cardano_database_detail_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Ok(Some(CardanoDatabaseSnapshotMessage::dummy())))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{hash}";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
    async fn test_cardano_database_detail_returns_404_not_found_when_no_cardano_database_snapshot()
    {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{hash}";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_database_detail_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{hash}";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
    async fn test_cardano_database_get_digests_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_digest_list_message()
            .return_once(|| Ok(vec![CardanoDatabaseDigestListItemMessage::dummy()]))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/digests";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
    async fn test_cardano_database_get_digests_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_digest_list_message()
            .return_once(|| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/digests";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
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
