use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;
use warp::Filter;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_full_immutable_snapshots(router_state)
        .or(artifact_cardano_full_immutable_snapshot_by_id(router_state))
        .or(serve_snapshots_dir(router_state))
        .or(snapshot_download(router_state))
}

/// GET /artifact/snapshots
fn artifact_cardano_full_immutable_snapshots(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshots")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_http_message_service(router_state))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/snapshot/:id
fn artifact_cardano_full_immutable_snapshot_by_id(
    dependency_manager: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String)
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and(middlewares::with_metrics_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

/// GET /artifact/snapshots/{digest}/download
fn snapshot_download(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String / "download")
        .and(warp::get().or(warp::head()).unify())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.server_url.clone()
        }))
        .and(middlewares::with_signed_entity_service(router_state))
        .and_then(handlers::snapshot_download)
}

fn serve_snapshots_dir(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path("snapshot_download")
        .and(warp::fs::dir(
            router_state.configuration.snapshot_directory.clone(),
        ))
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_signed_entity_service(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.allow_http_serve_directory
        }))
        .and_then(handlers::ensure_downloaded_file_is_a_snapshot)
}

mod handlers {
    use reqwest::Url;
    use slog::{debug, warn, Logger};
    use std::convert::Infallible;
    use std::str::FromStr;
    use std::sync::Arc;
    use warp::http::{StatusCode, Uri};

    use mithril_common::StdResult;

    use crate::http_server::routes::reply;
    use crate::services::{MessageService, SignedEntityService};
    use crate::tools::url_sanitizer;
    use crate::{unwrap_to_internal_server_error, MetricsService};

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List Snapshot artifacts
    pub async fn list_artifacts(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match http_message_service
            .get_snapshot_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"list_artifacts_snapshot"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Get Artifact by signed entity id
    pub async fn get_artifact_by_signed_entity_id(
        signed_entity_id: String,
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_artifact_detail_cardano_immutable_files_full_total_served_since_startup()
            .increment();

        match http_message_service
            .get_snapshot_message(&signed_entity_id)
            .await
        {
            Ok(Some(signed_entity)) => Ok(reply::json(&signed_entity, StatusCode::OK)),
            Ok(None) => {
                warn!(logger, "snapshot_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger,"snapshot_details::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Download a file if and only if it's a snapshot archive
    pub async fn ensure_downloaded_file_is_a_snapshot(
        reply: warp::fs::File,
        logger: Logger,
        signed_entity_service: Arc<dyn SignedEntityService>,
        allow_http_serve_directory: bool,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            logger,
            ">> ensure_downloaded_file_is_a_snapshot / file: `{}`",
            filepath.display()
        );

        if !allow_http_serve_directory {
            warn!(logger, "ensure_downloaded_file_is_a_snapshot::error"; "error" => "http serve directory is disabled");
            return Ok(reply::empty(StatusCode::FORBIDDEN));
        }

        match crate::tools::extract_digest_from_path(&filepath) {
            Ok(digest) => match signed_entity_service
                .get_signed_snapshot_by_id(&digest)
                .await
            {
                Ok(Some(_)) => Ok(reply::add_content_disposition_header(reply, &filepath)),
                _ => Ok(reply::empty(StatusCode::NOT_FOUND)),
            },
            Err(err) => {
                warn!(logger,"ensure_downloaded_file_is_a_snapshot::error"; "error" => ?err);
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
        }
    }

    /// Snapshot download
    pub async fn snapshot_download(
        digest: String,
        logger: Logger,
        server_url: String,
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match signed_entity_service
            .get_signed_snapshot_by_id(&digest)
            .await
        {
            Ok(Some(signed_entity)) => {
                let snapshot = signed_entity.artifact;
                let filename = format!(
                    "{}-e{}-i{}.{}.{}",
                    snapshot.network,
                    snapshot.beacon.epoch,
                    snapshot.beacon.immutable_file_number,
                    snapshot.digest,
                    snapshot.compression_algorithm.tar_file_extension()
                );
                let snapshot_uri = unwrap_to_internal_server_error!(
                    absolute_snapshot_uri(&server_url, &filename),
                    logger => "snapshot_download::error"
                );

                Ok(Box::new(warp::redirect::found(snapshot_uri)) as Box<dyn warp::Reply>)
            }
            Ok(None) => {
                warn!(logger, "snapshot_download::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger,"snapshot_download::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    fn absolute_snapshot_uri(server_url: &str, filename: &str) -> StdResult<Uri> {
        // warp requires an `Uri` from the `http` crate, but it does not have a 'join' method
        let sanitized_url = url_sanitizer::sanitize_url_path(&Url::parse(server_url)?)?
            .join(&format!("snapshot_download/{filename}"))?;
        let snapshot_uri = Uri::from_str(sanitized_url.as_str())?;
        Ok(snapshot_uri)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::http_server::routes::artifact_routes::test_utils::*;
    use crate::http_server::routes::router::RouterConfig;
    use crate::{
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        services::{MockMessageService, MockSignedEntityService},
    };
    use mithril_common::messages::{SnapshotListItemMessage, SnapshotMessage};
    use mithril_common::{
        entities::{CardanoDbBeacon, SignedEntityType, Snapshot},
        test_utils::{apispec::APISpec, fake_data},
    };
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
    async fn test_snapshots_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_snapshot_list_message()
            .return_once(|_| Ok(vec![SnapshotListItemMessage::dummy()]))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshots";

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
    async fn test_snapshots_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_snapshot_list_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshots";

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
    async fn test_snapshot_digest_increments_artifact_detail_total_served_since_startup_metric() {
        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";
        let dependency_manager = Arc::new(initialize_dependencies().await);
        let initial_counter_value = dependency_manager
            .metrics_service
            .get_artifact_detail_cardano_immutable_files_full_total_served_since_startup()
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
                .get_artifact_detail_cardano_immutable_files_full_total_served_since_startup()
                .get()
        );
    }

    #[tokio::test]
    async fn test_snapshot_digest_get_ok() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_snapshot_message()
            .return_once(|_| Ok(Some(SnapshotMessage::dummy())))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";

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
    async fn test_snapshot_digest_returns_404_not_found_when_no_snapshot() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_snapshot_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";

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
    async fn test_snapshot_digest_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_snapshot_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";

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
    async fn test_snapshot_local_download_returns_302_found_when_the_snapshot_exists() {
        let network = "devnet";
        let signed_entity = create_signed_entity(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default()),
            Snapshot {
                beacon: CardanoDbBeacon::new(1, 10),
                ..fake_data::snapshots(1)[0].clone()
            },
        );
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_signed_snapshot_by_id()
            .return_once(|_| Ok(Some(signed_entity)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(RouterState::new(
                Arc::new(dependency_manager),
                RouterConfig {
                    // Note: the trailing slash allows to test that the URL does not contain a double slash
                    server_url: "https://1.2.3.4:5552/".to_string(),
                    ..RouterConfig::dummy()
                },
            )))
            .await;

        assert_eq!(response.status(), StatusCode::FOUND);
        let location = std::str::from_utf8(response.headers()["location"].as_bytes())
            .unwrap()
            .to_string();
        assert!(
            location.contains(&format!("https://1.2.3.4:5552/snapshot_download/{network}")),
            "Expected value 'https://1.2.3.4:5552/snapshot_download/testnet' not found in {location}",
        );
    }

    #[tokio::test]
    async fn test_snapshot_download_returns_404_not_found_when_no_snapshot() {
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_signed_snapshot_by_id()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}/download";

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
            "application/gzip",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ko() {
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_signed_snapshot_by_id()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}/download";

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
