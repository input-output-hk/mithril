use axum::{
    body::Body,
    extract::{Path, State},
    http::{Response, StatusCode},
    response::IntoResponse,
};

use crate::shared_state::SharedState;
use crate::AppError;

pub async fn epoch_settings(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let epoch_settings = app_state.get_epoch_settings().await?;

    Ok(epoch_settings)
}

pub async fn snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
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

pub async fn msds(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let msds = app_state.get_msds().await?;

    Ok(msds)
}

pub async fn msd(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_msd(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("mithril stake distribution epoch={key}")))
}

pub async fn certificates(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let certificates = app_state.get_certificates().await?;

    Ok(certificates)
}

pub async fn certificate(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_certificate(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("certificate hash={key}")))
}

pub async fn statistics() -> Result<Response<Body>, AppError> {
    let response = Response::builder().status(StatusCode::CREATED);

    response.body(String::new().into()).map_err(|e| e.into())
}
