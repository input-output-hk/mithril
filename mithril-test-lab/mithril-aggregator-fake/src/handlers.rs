//! HTTP handlers module
//! This module contains the controllers for the different routes and middlewares.

use axum::{
    body::Body,
    extract::{Path, Request, State},
    http::{HeaderValue, Response, StatusCode},
    middleware::{from_fn, Next},
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use tower_http::{
    cors::CorsLayer,
    trace::{DefaultMakeSpan, DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::Level;

use crate::shared_state::SharedState;
use crate::AppError;

pub async fn aggregator_router() -> Router<SharedState> {
    Router::new()
        .route("/epoch-settings", get(epoch_settings))
        .route("/artifact/snapshots", get(snapshots))
        .route("/artifact/mithril-stake-distributions", get(msds))
        .route("/artifact/mithril-stake-distribution/:digest", get(msd))
        .route("/artifact/snapshot/:digest", get(snapshot))
        .route("/artifact/cardano-transactions", get(cardano_transactions))
        .route("/certificates", get(certificates))
        .route("/certificate/:hash", get(certificate))
        .route("/statistics/snapshot", post(statistics))
        .layer(CorsLayer::permissive())
        .layer(from_fn(set_json_app_header))
        .layer(
            TraceLayer::new_for_http()
                .make_span_with(
                    DefaultMakeSpan::new()
                        .include_headers(false)
                        .level(Level::DEBUG),
                )
                .on_request(DefaultOnRequest::new().level(Level::DEBUG))
                .on_response(
                    DefaultOnResponse::new()
                        .level(Level::INFO)
                        .include_headers(true)
                        .latency_unit(LatencyUnit::Micros),
                ),
        )
}

/// HTTP: Return the Epoch Settings.
pub async fn epoch_settings(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let epoch_settings = app_state.get_epoch_settings().await?;

    Ok(epoch_settings)
}

/// HTTP: Return a snapshot identified by its digest.
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

/// HTTP: return the list of snapshots.
pub async fn snapshots(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let snapshots = app_state.get_snapshots().await?;

    Ok(snapshots)
}

/// HTTP: return the list of mithril stake distributions.
pub async fn msds(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let msds = app_state.get_msds().await?;

    Ok(msds)
}

/// HTTP: return a mithril stake distribution identified by its hash.
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

/// HTTP: return the list of certificates
pub async fn certificates(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let certificates = app_state.get_certificates().await?;

    Ok(certificates)
}

/// HTTP: return a certificate identified by its hash.
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

/// HTTP: return OK when the client registers download statistics
pub async fn statistics() -> Result<Response<Body>, AppError> {
    let response = Response::builder().status(StatusCode::CREATED);

    response.body(String::new().into()).map_err(|e| e.into())
}

/// MIDDLEWARE: set JSON application type in HTTP headers
pub async fn set_json_app_header(
    req: Request,
    next: Next,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    let mut res = next.run(req).await;

    if res.status() == StatusCode::OK {
        let headers = res.headers_mut();
        headers.insert(
            "Content-Type",
            HeaderValue::from_static("application/json; charset=utf-8"),
        );
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::shared_state::AppState;

    pub use super::*;

    #[tokio::test]
    async fn invalid_snapshot_digest() {
        let state: State<SharedState> = State(AppState::default().into());
        let digest = Path("whatever".to_string());

        let error = snapshot(digest, state).await.expect_err(
            "The handler was expected to fail since the snapshot's digest does not exist.",
        );

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_snapshot_digest() {
        let state: State<SharedState> = State(AppState::default().into());
        let digest =
            Path("000ee4c84c7b64a62dc30ec78a765a1f3bb81cd9dd4bd1eccf9f2da785e70877".to_string());

        let response = snapshot(digest, state)
            .await
            .expect("The handler was expected to succeed since the snapshot's digest does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_msd_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = msd(hash, state)
            .await
            .expect_err("The handler was expected to fail since the msd's hash does not exist.");

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_certificate_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash =
            Path("8f4e859b16774da9a57926d7af226bfe0a655a8e309ae4be234d0e776eb4a59f".to_string());

        let response = certificate(hash, state)
            .await
            .expect("The handler was expected to succeed since the certificate's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_certificate_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = certificate(hash, state).await.expect_err(
            "The handler was expected to fail since the certificate's hash does not exist.",
        );

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_msd_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash =
            Path("03ebb00e6626037f2e58eb7cc50d308fd57c253baa1fe2b04eb5945ced16b5bd".to_string());

        let response = msd(hash, state)
            .await
            .expect("The handler was expected to succeed since the msd's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }
}
