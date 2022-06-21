use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    snapshots(dependency_manager.clone())
        .or(serve_snapshots_dir(dependency_manager.clone()))
        .or(snapshot_download(dependency_manager.clone()))
        .or(snapshot_digest(dependency_manager))
}

/// GET /snapshots
fn snapshots(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("snapshots")
        .and(warp::get())
        .and(middlewares::with_snapshot_store(dependency_manager))
        .and_then(handlers::snapshots)
}

/// GET /snapshots/{digest}/download
fn snapshot_download(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("snapshot" / String / "download")
        .and(warp::get())
        .and(middlewares::with_config(dependency_manager.clone()))
        .and(middlewares::with_snapshot_store(dependency_manager))
        .and_then(handlers::snapshot_download)
}

fn serve_snapshots_dir(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    let config = dependency_manager.config.clone();

    warp::path("snapshot_download")
        .and(warp::fs::dir(config.snapshot_directory))
        .and(middlewares::with_snapshot_store(dependency_manager))
        .and_then(handlers::ensure_downloaded_file_is_a_snapshot)
}

/// GET /snapshot/digest
fn snapshot_digest(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("snapshot" / String)
        .and(warp::get())
        .and(middlewares::with_snapshot_store(dependency_manager))
        .and_then(handlers::snapshot_digest)
}

mod handlers {
    use crate::dependency::SnapshotStoreWrapper;
    use crate::http_server::routes::reply;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::Config;
    use slog_scope::debug;
    use std::convert::Infallible;
    use std::str::FromStr;
    use warp::http::{StatusCode, Uri};

    /// Snapshots
    pub async fn snapshots(
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshots");

        // Snapshots
        let snapshot_store = snapshot_store.read().await;
        match snapshot_store.list_snapshots().await {
            Ok(snapshots) => Ok(reply::json(&snapshots, StatusCode::OK)),
            Err(err) => Ok(reply::internal_server_error(err.to_string())),
        }
    }

    /// Download a file if and only if it's a snapshot archive
    pub async fn ensure_downloaded_file_is_a_snapshot(
        reply: warp::fs::File,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            "ensure_downloaded_file_is_a_snapshot / file: `{}`",
            filepath.display()
        );

        match crate::tools::extract_digest_from_path(&filepath) {
            Ok(digest) => {
                let snapshot_store = snapshot_store.read().await;
                match snapshot_store.get_snapshot_details(digest).await {
                    Ok(Some(_)) => Ok(Box::new(warp::reply::with_header(
                        reply,
                        "Content-Disposition",
                        format!(
                            "attachment; filename=\"{}\"",
                            filepath.file_name().unwrap().to_str().unwrap()
                        ),
                    )) as Box<dyn warp::Reply>),
                    _ => Ok(reply::empty(StatusCode::NOT_FOUND)),
                }
            }
            Err(_) => Ok(reply::empty(StatusCode::NOT_FOUND)),
        }
    }

    /// Snapshot download
    pub async fn snapshot_download(
        digest: String,
        config: Config,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_download/{}", digest);

        // Snapshot
        let snapshot_store = snapshot_store.read().await;
        match snapshot_store.get_snapshot_details(digest).await {
            Ok(Some(snapshot)) => {
                let filename = format!("{}.{}.tar.gz", config.network, snapshot.digest);
                let snapshot_uri = format!(
                    "{}{}/snapshot_download/{}",
                    config.server_url, SERVER_BASE_PATH, filename
                );
                let snapshot_uri = Uri::from_str(&snapshot_uri).unwrap();

                Ok(Box::new(warp::redirect::found(snapshot_uri)) as Box<dyn warp::Reply>)
            }
            Ok(None) => Ok(reply::empty(StatusCode::NOT_FOUND)),
            Err(err) => Ok(reply::internal_server_error(err.to_string())),
        }
    }

    /// Snapshot by digest
    pub async fn snapshot_digest(
        digest: String,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_digest/{}", digest);

        // Snapshot
        let snapshot_store = snapshot_store.read().await;
        match snapshot_store.get_snapshot_details(digest).await {
            Ok(snapshot) => match snapshot {
                Some(snapshot) => Ok(reply::json(&snapshot, StatusCode::OK)),
                None => Ok(reply::empty(StatusCode::NOT_FOUND)),
            },
            Err(err) => Ok(reply::internal_server_error(err.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::apispec::APISpec;
    use mithril_common::fake_data;
    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::snapshot_stores::{MockSnapshotStore, SnapshotStoreError};

    fn setup_dependency_manager() -> DependencyManager {
        DependencyManager::fake()
    }

    fn setup_router(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
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
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshots";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
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
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshots";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ok() {
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .content_type("application/gzip")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ok_nosnapshot() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ko() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Err(SnapshotStoreError::Manifest(
                "an error occurred".to_string(),
            )))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_digest_get_ok() {
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_digest_get_ok_nosnapshot() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
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
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
