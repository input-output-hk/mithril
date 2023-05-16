use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_mithril_stake_distributions(dependency_manager.clone()).or(
        artifact_mithril_stake_distribution_by_id(dependency_manager),
    )
}

/// GET /artifact/mithril-stake-distributions
fn artifact_mithril_stake_distributions(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distributions")
        .and(warp::get())
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/mithril-stake-distribution/:id
fn artifact_mithril_stake_distribution_by_id(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distribution" / String)
        .and(warp::get())
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

pub mod handlers {
    use crate::http_server::routes::reply;
    use crate::message_adapters::{
        ToMithrilStakeDistributionListMessageAdapter, ToMithrilStakeDistributionMessageAdapter,
    };
    use crate::SignedEntityService;
    use mithril_common::messages::MessageAdapter;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List MithrilStakeDistribution artifacts
    pub async fn list_artifacts(
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifacts");

        match signed_entity_service
            .get_last_signed_mithril_stake_distribution(LIST_MAX_ITEMS)
            .await
        {
            Ok(signed_entities) => {
                let messages = ToMithrilStakeDistributionListMessageAdapter::adapt(signed_entities);
                Ok(reply::json(&messages, StatusCode::OK))
            }
            Err(err) => {
                warn!("artifacts_mithril_stake_distribution"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    /// Get Artifact by signed entity id
    pub async fn get_artifact_by_signed_entity_id(
        signed_entity_id: String,
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifact/{signed_entity_id}");

        match signed_entity_service
            .get_signed_mithril_stake_distribution_by_id(&signed_entity_id)
            .await
        {
            Ok(Some(signed_entity)) => {
                let message = ToMithrilStakeDistributionMessageAdapter::adapt(signed_entity);
                Ok(reply::json(&message, StatusCode::OK))
            }
            Ok(None) => {
                warn!("mithril_stake_distribution_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!("mithril_stake_distribution_details::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::sqlite::HydrationError;
    use mithril_common::test_utils::apispec::APISpec;
    use mithril_common::test_utils::fake_data;
    use serde_json::Value::Null;

    use crate::initialize_dependencies;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::database::provider::MockSignedEntityStorer;

    fn setup_router(
        dependency_manager: Arc<DependencyManager>,
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
    async fn test_mithril_stake_distributions_get_ok() {
        let signed_entity_records = shared::tests::create_signed_entity_records(
            SignedEntityType::MithrilStakeDistribution(Epoch::default()),
            fake_data::mithril_stake_distributions(5),
        );
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_last_signed_stake_distribution()
            .return_once(|_, _| Ok(signed_entity_records))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

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
        );
    }

    #[tokio::test]
    async fn test_mithril_stake_distributions_get_ko() {
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

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
        );
    }

    #[tokio::test]
    async fn test_mithril_stake_distribution_get_ok() {
        let signed_entity_record = shared::tests::create_signed_entity_records(
            SignedEntityType::MithrilStakeDistribution(Epoch::default()),
            fake_data::mithril_stake_distributions(1),
        )
        .first()
        .unwrap()
        .to_owned();
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(Some(signed_entity_record)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

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
        );
    }

    #[tokio::test]
    async fn test_mithril_stake_distribution_ok_norecord() {
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

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
        );
    }

    #[tokio::test]
    async fn test_mithril_stake_distribution_get_ko() {
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

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
        );
    }
}
