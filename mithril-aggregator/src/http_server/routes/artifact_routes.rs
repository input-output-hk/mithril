use crate::artifact_builder::{MithrilStakeDistribution, MithrilStakeDistributionSummary};
use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use mithril_common::entities::{Beacon, Epoch, SignedEntityType, Snapshot};
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_mithril_stake_distributions(dependency_manager.clone())
        .or(artifact_mithril_stake_distribution_by_id(
            dependency_manager.clone(),
        ))
        .or(artifact_cardano_full_immutable_snapshots(
            dependency_manager.clone(),
        ))
        .or(artifact_cardano_full_immutable_snapshot_by_id(
            dependency_manager,
        ))
}

/// GET /artifact/mithril-stake-distributions
fn artifact_mithril_stake_distributions(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distributions")
        .and(warp::get())
        .and(middlewares::with_signed_entity_storer(dependency_manager))
        .and(middlewares::with_signed_entity_type(
            SignedEntityType::MithrilStakeDistribution(Epoch::default()),
        ))
        .and_then(handlers::artifacts_by_signed_entity_type::<MithrilStakeDistributionSummary>)
}

/// GET /artifact/mithril-stake-distribution/:id
fn artifact_mithril_stake_distribution_by_id(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "mithril-stake-distribution" / String)
        .and(warp::get())
        .and(middlewares::with_signed_entity_storer(dependency_manager))
        .and_then(handlers::artifact_by_signed_entity_id::<MithrilStakeDistribution>)
}

/// GET /artifact/snapshots
fn artifact_cardano_full_immutable_snapshots(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshots")
        .and(warp::get())
        .and(middlewares::with_signed_entity_storer(dependency_manager))
        .and(middlewares::with_signed_entity_type(
            SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
        ))
        .and_then(handlers::artifacts_by_signed_entity_type::<Snapshot>)
}

/// GET /artifact/snapshot/:id
fn artifact_cardano_full_immutable_snapshot_by_id(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String)
        .and(warp::get())
        .and(middlewares::with_signed_entity_storer(dependency_manager))
        .and_then(handlers::artifact_by_signed_entity_id::<Snapshot>)
}

mod handlers {
    use crate::database::provider::SignedEntityStorer;
    use crate::http_server::routes::reply;
    use mithril_common::entities::SignedEntityType;
    use serde::{Deserialize, Serialize};
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// Artifacts by signed entity type
    pub async fn artifacts_by_signed_entity_type<T: Serialize + for<'a> Deserialize<'a>>(
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
        signed_entity_type: SignedEntityType,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifacts");

        match signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type, LIST_MAX_ITEMS)
            .await
        {
            Ok(signed_entities) => {
                let mut artifacts = Vec::new();
                for signed_entity in signed_entities {
                    if let Ok(artifact) = serde_json::from_str::<T>(&signed_entity.artifact) {
                        artifacts.push(artifact)
                    }
                }
                Ok(reply::json(&artifacts, StatusCode::OK))
            }
            Err(err) => {
                warn!("artifacts_by_signed_entity_type"; "signed_entity_type" => ?signed_entity_type, "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    /// Artifact by signed entity id
    pub async fn artifact_by_signed_entity_id<T: Serialize + for<'a> Deserialize<'a>>(
        signed_entity_id: String,
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifact/{signed_entity_id}");

        match signed_entity_storer
            .get_signed_entity(signed_entity_id.clone())
            .await
        {
            Ok(signed_entity) => match signed_entity {
                Some(signed_entity) => match serde_json::from_str::<T>(&signed_entity.artifact) {
                    Ok(artifact) => Ok(reply::json(&artifact, StatusCode::OK)),
                    Err(err) => Ok(reply::internal_server_error(err.to_string())),
                },
                None => Ok(reply::empty(StatusCode::NOT_FOUND)),
            },
            Err(err) => {
                warn!("artifact_by_signed_entity_id"; "signed_entity_id" => ?signed_entity_id, "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

/* #[cfg(test)]
mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::test_utils::apispec::APISpec;
    use mithril_common::test_utils::fake_data;
    use serde_json::Value::Null;

    use crate::initialize_dependencies;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::snapshot_stores::{MockSnapshotStore, SnapshotStoreError};

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
    async fn test_snapshots_get_ok() {
        let fake_snapshots = fake_data::snapshots(5);
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_list_snapshots()
            .return_const(Ok(fake_snapshots))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.snapshot_store = Arc::new(mock_snapshot_store);

        let method = Method::GET.as_str();
        let path = "/snapshots";

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
    async fn test_snapshots_get_ko() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_list_snapshots()
            .return_const(Err(SnapshotStoreError::Manifest(
                "an error occurred".to_string(),
            )))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.snapshot_store = Arc::new(mock_snapshot_store);

        let method = Method::GET.as_str();
        let path = "/snapshots";

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
    async fn test_snapshot_digest_get_ok() {
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.snapshot_store = Arc::new(mock_snapshot_store);

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
    async fn test_snapshot_digest_get_ok_nosnapshot() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.snapshot_store = Arc::new(mock_snapshot_store);

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
    async fn test_snapshot_digest_get_ko() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Err(SnapshotStoreError::Manifest(
                "an error occurred".to_string(),
            )))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.snapshot_store = Arc::new(mock_snapshot_store);

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
} */
