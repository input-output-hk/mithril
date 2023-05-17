use crate::http_server::routes::middlewares;
use crate::http_server::SERVER_BASE_PATH;
use crate::DependencyManager;
use std::sync::Arc;
use warp::hyper::Uri;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_full_immutable_snapshots(dependency_manager.clone())
        .or(artifact_cardano_full_immutable_snapshot_by_id(
            dependency_manager.clone(),
        ))
        .or(serve_snapshots_dir(dependency_manager.clone()))
        .or(snapshot_download(dependency_manager))
        .or(artifact_cardano_full_immutable_snapshots_legacy())
        .or(artifact_cardano_full_immutable_snapshot_by_id_legacy())
}

/// GET /artifact/snapshots
fn artifact_cardano_full_immutable_snapshots(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshots")
        .and(warp::get())
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/snapshot/:id
fn artifact_cardano_full_immutable_snapshot_by_id(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String)
        .and(warp::get())
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

/// GET /snapshots/{digest}/download
fn snapshot_download(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "snapshot" / String / "download")
        .and(warp::get())
        .and(middlewares::with_config(dependency_manager.clone()))
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::snapshot_download)
}

fn serve_snapshots_dir(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    let config = dependency_manager.config.clone();

    warp::path("snapshot_download")
        .and(warp::fs::dir(config.snapshot_directory))
        .and(middlewares::with_signed_entity_service(dependency_manager))
        .and_then(handlers::ensure_downloaded_file_is_a_snapshot)
}

/// GET /snapshots
// TODO: This legacy route should be removed when this code is released with a new distribution
fn artifact_cardano_full_immutable_snapshots_legacy(
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("snapshots").map(|| {
        warp::redirect(
            format!("/{SERVER_BASE_PATH}/artifact/snapshots")
                .as_str()
                .parse::<Uri>()
                .unwrap(),
        )
    })
}

/// GET /snapshot/digest
// TODO: This legacy route should be removed when this code is released with a new distribution
fn artifact_cardano_full_immutable_snapshot_by_id_legacy(
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("snapshot" / String).map(|digest| {
        warp::redirect(
            format!("/{SERVER_BASE_PATH}/artifact/snapshot/{digest}")
                .as_str()
                .parse::<Uri>()
                .unwrap(),
        )
    })
}

mod handlers {
    use crate::http_server::routes::reply;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::message_adapters::ToSnapshotListMessageAdapter;
    use crate::message_adapters::ToSnapshotMessageAdapter;
    use crate::{signed_entity_service::SignedEntityService, Configuration};
    use mithril_common::messages::MessageAdapter;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::str::FromStr;
    use std::sync::Arc;
    use warp::http::{StatusCode, Uri};

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List Snaptshot artifacts
    pub async fn list_artifacts(
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifacts");

        match signed_entity_service
            .get_last_signed_snapshots(LIST_MAX_ITEMS)
            .await
        {
            Ok(signed_entities) => {
                let messages = ToSnapshotListMessageAdapter::adapt(signed_entities);
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
            .get_signed_snapshot_by_id(&signed_entity_id)
            .await
        {
            Ok(Some(signed_entity)) => {
                let message = ToSnapshotMessageAdapter::adapt(signed_entity);
                Ok(reply::json(&message, StatusCode::OK))
            }
            Ok(None) => {
                warn!("snapshot_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!("snapshot_details::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    /// Download a file if and only if it's a snapshot archive
    pub async fn ensure_downloaded_file_is_a_snapshot(
        reply: warp::fs::File,
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            "⇄ HTTP SERVER: ensure_downloaded_file_is_a_snapshot / file: `{}`",
            filepath.display()
        );

        match crate::tools::extract_digest_from_path(&filepath) {
            Ok(digest) => match signed_entity_service
                .get_signed_snapshot_by_id(&digest)
                .await
            {
                Ok(Some(_)) => Ok(Box::new(warp::reply::with_header(
                    reply,
                    "Content-Disposition",
                    format!(
                        "attachment; filename=\"{}\"",
                        filepath.file_name().unwrap().to_str().unwrap()
                    ),
                )) as Box<dyn warp::Reply>),
                _ => Ok(reply::empty(StatusCode::NOT_FOUND)),
            },
            Err(err) => {
                warn!("ensure_downloaded_file_is_a_snapshot::error"; "error" => ?err);
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
        }
    }

    /// Snapshot download
    pub async fn snapshot_download(
        digest: String,
        config: Configuration,
        signed_entity_service: Arc<dyn SignedEntityService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: snapshot_download/{}", digest);

        match signed_entity_service
            .get_signed_snapshot_by_id(&digest)
            .await
        {
            Ok(Some(signed_entity)) => {
                let snapshot = signed_entity.artifact;
                let filename = format!(
                    "{}-e{}-i{}.{}.tar.gz",
                    snapshot.beacon.network,
                    snapshot.beacon.epoch,
                    snapshot.beacon.immutable_file_number,
                    snapshot.digest
                );
                let snapshot_uri = format!(
                    "{}{}/snapshot_download/{}",
                    config.get_server_url(),
                    SERVER_BASE_PATH,
                    filename
                );
                let snapshot_uri = Uri::from_str(&snapshot_uri).unwrap();

                Ok(Box::new(warp::redirect::found(snapshot_uri)) as Box<dyn warp::Reply>)
            }
            Ok(None) => {
                warn!("snapshot_download::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!("snapshot_download::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::entities::SignedEntityType;
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
    async fn test_snapshots_get_ok() {
        let signed_entity_records = shared::tests::create_signed_entity_records(
            SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
            fake_data::snapshots(5),
        );
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Ok(signed_entity_records))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshots";

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
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshots";

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
        let signed_entity_record = shared::tests::create_signed_entity_records(
            SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
            fake_data::snapshots(1),
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
        let path = "/artifact/snapshot/{digest}";

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
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";

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
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}";

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
    async fn test_snapshot_download_get_ok() {
        let signed_entity_record = shared::tests::create_signed_entity_records(
            SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
            fake_data::snapshots(1),
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
        let path = "/artifact/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/gzip",
            &Null,
            &response,
        );
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ok_nosnapshot() {
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/gzip",
            &Null,
            &response,
        );
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ko() {
        let mut mock_signed_entity_storer = MockSignedEntityStorer::new();
        mock_signed_entity_storer
            .expect_get_signed_entity()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signed_entity_storer = Arc::new(mock_signed_entity_storer);

        let method = Method::GET.as_str();
        let path = "/artifact/snapshot/{digest}/download";

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
