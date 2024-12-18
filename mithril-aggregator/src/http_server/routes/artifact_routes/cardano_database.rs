use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;
use warp::Filter;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_database_list(router_state)
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
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and(middlewares::with_metrics_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

fn serve_cardano_database_dir(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path("cardano_database_download")
        .and(warp::fs::dir(
            router_state.configuration.snapshot_directory.clone(),
        ))
        .and(middlewares::with_logger(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.allow_http_serve_directory
        }))
        .and_then(handlers::ensure_downloaded_file_is_a_cardano_database_artifact)
}

mod handlers {
    use crate::http_server::routes::reply;
    use crate::services::MessageService;
    use crate::MetricsService;
    use slog::{debug, warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

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
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_artifact_detail_cardano_db_total_served_since_startup()
            .increment();

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
    pub async fn ensure_downloaded_file_is_a_cardano_database_artifact(
        reply: warp::fs::File,
        logger: Logger,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            logger,
            ">> ensure_downloaded_file_is_a_cardano_database / file: `{}`",
            filepath.display()
        );

        // TODO: enhance this check with a regular expression once the file naming convention is defined
        let file_is_a_cardano_database_archive = filepath.to_string_lossy().contains("ancillary")
            || filepath.to_string_lossy().contains("immutable");
        match file_is_a_cardano_database_archive {
            true => Ok(Box::new(warp::reply::with_header(
                reply,
                "Content-Disposition",
                format!(
                    "attachment; filename=\"{}\"",
                    filepath.file_name().unwrap().to_str().unwrap()
                ),
            )) as Box<dyn warp::Reply>),
            false => {
                warn!(logger,"ensure_downloaded_file_is_a_cardano_database::error"; "error" => "file is not a Cardano database archive");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        http_server::SERVER_BASE_PATH, initialize_dependencies, services::MockMessageService,
    };
    use mithril_common::messages::{
        CardanoDatabaseSnapshotListItemMessage, CardanoDatabaseSnapshotMessage,
    };
    use mithril_common::test_utils::apispec::APISpec;
    use mithril_persistence::sqlite::HydrationError;
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

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
    async fn test_cardano_database_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_list_message()
            .return_once(|_| Ok(vec![CardanoDatabaseSnapshotListItemMessage::dummy()]))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database";

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
    async fn test_cardano_database_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_list_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database";

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
    async fn test_cardano_database_detail_increments_artifact_detail_total_served_since_startup_metric(
    ) {
        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{merkle_root}";
        let dependency_manager = Arc::new(initialize_dependencies().await);
        let initial_counter_value = dependency_manager
            .metrics_service
            .get_artifact_detail_cardano_db_total_served_since_startup()
            .get();

        request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(
                dependency_manager.clone(),
            )))
            .await;

        assert_eq!(
            initial_counter_value + 1,
            dependency_manager
                .metrics_service
                .get_artifact_detail_cardano_db_total_served_since_startup()
                .get()
        );
    }

    #[tokio::test]
    async fn test_cardano_database_detail_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Ok(Some(CardanoDatabaseSnapshotMessage::dummy())))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{merkle_root}";

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
    async fn test_cardano_database_detail_returns_404_not_found_when_no_cardano_database_snapshot()
    {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{merkle_root}";

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
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_database_digest_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_database_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-database/{merkle_root}";

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
}
