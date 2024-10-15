use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use warp::Filter;

pub fn routes(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_mithril_stake_distributions(dependency_manager).or(
        artifact_mithril_stake_distribution_by_id(dependency_manager),
    )
}

/// GET /artifact/mithril-stake-distributions
fn artifact_mithril_stake_distributions(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distributions")
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/mithril-stake-distribution/:id
fn artifact_mithril_stake_distribution_by_id(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distribution" / String)
        .and(warp::get())
        .and(middlewares::with_logger(dependency_manager))
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

pub mod handlers {
    use crate::http_server::routes::reply;
    use crate::services::MessageService;

    use slog::{warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List MithrilStakeDistribution artifacts
    pub async fn list_artifacts(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match http_message_service
            .get_mithril_stake_distribution_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"list_artifacts_mithril_stake_distribution"; "error" => ?err);
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
        match http_message_service
            .get_mithril_stake_distribution_message(&signed_entity_id)
            .await
        {
            Ok(Some(message)) => Ok(reply::json(&message, StatusCode::OK)),
            Ok(None) => {
                warn!(logger, "get_mithril_stake_distribution_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger,"get_mithril_stake_distribution_details::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::http_server::routes::artifact_routes::test_utils::*;
    use crate::{
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        message_adapters::{
            ToMithrilStakeDistributionListMessageAdapter, ToMithrilStakeDistributionMessageAdapter,
        },
        services::MockMessageService,
    };
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        messages::ToMessageAdapter,
        test_utils::{apispec::APISpec, fake_data},
    };
    use mithril_persistence::sqlite::HydrationError;
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
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
    async fn test_mithril_stake_distributions_get_ok() {
        let signed_entity_records = create_signed_entities(
            SignedEntityType::MithrilStakeDistribution(Epoch::default()),
            fake_data::mithril_stake_distributions(5),
        );
        let message = ToMithrilStakeDistributionListMessageAdapter::adapt(signed_entity_records);
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_mithril_stake_distribution_list_message()
            .return_once(|_| Ok(message))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/mithril-stake-distributions";

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
    async fn test_mithril_stake_distributions_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_mithril_stake_distribution_list_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/mithril-stake-distributions";

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
    async fn test_mithril_stake_distribution_get_ok() {
        let signed_entity = create_signed_entities(
            SignedEntityType::MithrilStakeDistribution(Epoch::default()),
            fake_data::mithril_stake_distributions(1),
        )
        .first()
        .unwrap()
        .to_owned();
        let message = ToMithrilStakeDistributionMessageAdapter::adapt(signed_entity);
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_mithril_stake_distribution_message()
            .return_once(|_| Ok(Some(message)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/mithril-stake-distribution/{hash}";

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
    async fn test_mithril_stake_distribution_returns_404_no_found_when_no_record() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_mithril_stake_distribution_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/mithril-stake-distribution/{hash}";

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
    async fn test_mithril_stake_distribution_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_mithril_stake_distribution_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/mithril-stake-distribution/{hash}";

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
