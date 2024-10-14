use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use warp::Filter;

pub fn routes(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_stake_distributions(dependency_manager)
        .or(artifact_cardano_stake_distribution_by_id(
            dependency_manager,
        ))
        .or(artifact_cardano_stake_distribution_by_epoch(
            dependency_manager,
        ))
}

/// GET /artifact/cardano-stake-distributions
fn artifact_cardano_stake_distributions(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-stake-distributions")
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/cardano-stake-distribution/:id
fn artifact_cardano_stake_distribution_by_id(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-stake-distribution" / String)
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

/// GET /artifact/cardano-stake-distribution/epoch/:epoch
fn artifact_cardano_stake_distribution_by_epoch(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-stake-distribution" / "epoch" / String)
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::get_artifact_by_epoch)
}

pub mod handlers {
    use crate::http_server::routes::reply;
    use crate::services::MessageService;

    use mithril_common::entities::Epoch;
    use slog::{debug, warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List CardanoStakeDistribution artifacts
    pub async fn list_artifacts(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "⇄ HTTP SERVER: artifacts");

        match http_message_service
            .get_cardano_stake_distribution_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger, "list_artifacts_cardano_stake_distribution"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Get Artifact by signed entity id
    pub async fn get_artifact_by_signed_entity_id(
        signed_entity_id: String,
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "⇄ HTTP SERVER: artifact/{signed_entity_id}");

        match http_message_service
            .get_cardano_stake_distribution_message(&signed_entity_id)
            .await
        {
            Ok(Some(message)) => Ok(reply::json(&message, StatusCode::OK)),
            Ok(None) => {
                warn!(logger, "get_cardano_stake_distribution_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger, "get_cardano_stake_distribution_details::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Get Artifact by epoch
    pub async fn get_artifact_by_epoch(
        epoch: String,
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "⇄ HTTP SERVER: artifact/epoch/{epoch}");

        let artifact_epoch = match epoch.parse::<u64>() {
            Ok(epoch) => Epoch(epoch),
            Err(err) => {
                warn!(logger, "get_artifact_by_epoch::invalid_epoch"; "error" => ?err);
                return Ok(reply::bad_request(
                    "invalid_epoch".to_string(),
                    err.to_string(),
                ));
            }
        };

        match http_message_service
            .get_cardano_stake_distribution_message_by_epoch(artifact_epoch)
            .await
        {
            Ok(Some(message)) => Ok(reply::json(&message, StatusCode::OK)),
            Ok(None) => {
                warn!(
                    logger,
                    "get_cardano_stake_distribution_details_by_epoch::not_found"
                );
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger, "get_cardano_stake_distribution_details_by_epoch::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use anyhow::anyhow;
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        messages::{CardanoStakeDistributionListItemMessage, CardanoStakeDistributionMessage},
        test_utils::apispec::APISpec,
    };

    use crate::{
        http_server::SERVER_BASE_PATH, initialize_dependencies, services::MockMessageService,
    };

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
    async fn test_cardano_stake_distributions_returns_ok() {
        let message = vec![CardanoStakeDistributionListItemMessage::dummy()];
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_list_message()
            .return_once(|_| Ok(message))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-stake-distributions";

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
    async fn test_cardano_stake_distributions_returns_ko_500_when_error() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_list_message()
            .return_once(|_| Err(anyhow!("an error occured")))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-stake-distributions";

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

    #[tokio::test]
    async fn test_cardano_stake_distribution_returns_ok() {
        let message = CardanoStakeDistributionMessage::dummy();
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message()
            .return_once(|_| Ok(Some(message)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-stake-distribution/{hash}";

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
    async fn test_cardano_stake_distribution_returns_404_not_found_when_no_record() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-stake-distribution/{hash}";

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
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_returns_ko_500_when_error() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message()
            .return_once(|_| Err(anyhow!("an error occured")))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-stake-distribution/{hash}";

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

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_epoch_returns_ok() {
        let message = CardanoStakeDistributionMessage::dummy();
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message_by_epoch()
            .return_once(|_| Ok(Some(message)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let base_path = "/artifact/cardano-stake-distribution/epoch";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{base_path}/123"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_epoch_returns_400_bad_request_when_invalid_epoch() {
        let mock_http_message_service = MockMessageService::new();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let base_path = "/artifact/cardano-stake-distribution/epoch";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{base_path}/invalid-epoch"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::BAD_REQUEST,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_epoch_returns_404_not_found_when_no_record() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message_by_epoch()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let base_path = "/artifact/cardano-stake-distribution/epoch";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{base_path}/123"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_epoch_returns_ko_500_when_error() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_stake_distribution_message_by_epoch()
            .return_once(|_| Err(anyhow!("an error occured")))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let base_path = "/artifact/cardano-stake-distribution/epoch";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{base_path}/123"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }
}
