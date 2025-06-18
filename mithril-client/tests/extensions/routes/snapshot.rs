use std::sync::Arc;

use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};
use tokio::sync::RwLock;
use tower_http::services::fs::ServeDir;

use mithril_client::{Snapshot, SnapshotListItem};

#[derive(Debug, Clone)]
struct SnapshotRoutesState {
    snapshot_list: Vec<SnapshotListItem>,
    snapshot: Arc<RwLock<Snapshot>>,
}

pub fn routes(
    snapshot_list: Vec<SnapshotListItem>,
    snapshot: Arc<RwLock<Snapshot>>,
    snapshot_dir: &std::path::Path,
) -> Router {
    let state = SnapshotRoutesState {
        snapshot_list,
        snapshot,
    };

    Router::new()
        .route("/artifact/snapshots", get(snapshots))
        .route("/artifact/snapshot/{hash}", get(snapshot_by_hash))
        .nest_service("/snapshot_download", ServeDir::new(snapshot_dir))
        .with_state(state)
}

/// Route: /artifact/snapshots
async fn snapshots(state: State<SnapshotRoutesState>) -> Json<Vec<SnapshotListItem>> {
    Json(state.snapshot_list.clone())
}

/// Route: /artifact/snapshot/{hash}
async fn snapshot_by_hash(
    Path(_hash): Path<String>,
    state: State<SnapshotRoutesState>,
) -> Json<Snapshot> {
    Json(state.snapshot.read().await.clone())
}
