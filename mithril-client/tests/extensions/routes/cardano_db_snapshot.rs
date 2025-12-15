use std::sync::Arc;

use axum::extract::{Path, State};
use axum::routing::get;
use axum::{Json, Router};
use mithril_client::{CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem};
use tokio::sync::RwLock;
use tower_http::services::fs::ServeDir;

#[derive(Debug, Clone)]
struct CardanoDbSnapshotRoutesState {
    cardano_db_snapshot_list: Vec<CardanoDatabaseSnapshotListItem>,
    cardano_db_snapshot: Arc<RwLock<CardanoDatabaseSnapshot>>,
}

pub fn routes(
    cardano_db_snapshot_list: Vec<CardanoDatabaseSnapshotListItem>,
    cardano_db_snapshot: Arc<RwLock<CardanoDatabaseSnapshot>>,
    cardano_db_snapshot_archives_path: &std::path::Path,
) -> Router {
    let state = CardanoDbSnapshotRoutesState {
        cardano_db_snapshot_list,
        cardano_db_snapshot,
    };

    Router::new()
        .route("/artifact/cardano-database", get(cardano_db_snapshots))
        .route(
            "/artifact/cardano-database/{hash}",
            get(cardano_db_snapshot_by_hash),
        )
        .nest_service(
            "/cardano-database-download",
            ServeDir::new(cardano_db_snapshot_archives_path),
        )
        .with_state(state)
}

/// Route: /artifact/cardano-database
async fn cardano_db_snapshots(
    State(state): State<CardanoDbSnapshotRoutesState>,
) -> Json<Vec<CardanoDatabaseSnapshotListItem>> {
    Json(state.cardano_db_snapshot_list)
}

/// Route: /artifact/cardano-database/:hash
async fn cardano_db_snapshot_by_hash(
    Path(_hash): Path<String>,
    State(state): State<CardanoDbSnapshotRoutesState>,
) -> Json<CardanoDatabaseSnapshot> {
    Json(state.cardano_db_snapshot.read().await.clone())
}
