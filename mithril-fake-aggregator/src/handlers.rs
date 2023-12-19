use axum::{
    extract::{Path, State},
    response::{IntoResponse, Response},
    Json,
};

use crate::shared_state::{AppState, SharedState};
use crate::{AppError, StdResult};

pub async fn epoch_settings(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let epoch_settings = app_state.get_epoch_settings().await?;

    Ok(epoch_settings.into())
}

pub async fn snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response, AppError> {
    let app_state = state.read().await;

    app_state
        .get_snapshot(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("snapshot digest={key}")))
}

pub async fn snapshots(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let snapshots = app_state.get_snapshots().await?;

    Ok(snapshots)
}

pub async fn msds(State(state): State<SharedState>) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn msd(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn certificates(State(state): State<SharedState>) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn certificate(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Json<String>, AppError> {
    todo!()
}
