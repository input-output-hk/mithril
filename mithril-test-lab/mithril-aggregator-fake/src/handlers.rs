//! HTTP handlers module
//! This module contains the controllers for the different routes and middlewares.

use axum::{
    body::Body,
    extract::{Path, Query, Request, State},
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
use tracing::debug;
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
        .route("/artifact/cardano-transactions", get(ctx_snapshots))
        .route("/artifact/cardano-transaction/:hash", get(ctx_snapshot))
        .route("/proof/cardano-transaction", get(ctx_proof))
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
        .ok_or_else(|| {
            debug!("snapshot digest={key} NOT FOUND.");
            AppError::NotFound("".to_string())
        })
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
        .ok_or_else(|| {
            debug!("mithril stake distribution epoch={key} NOT FOUND.");
            AppError::NotFound("".to_string())
        })
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
        .ok_or_else(|| {
            debug!("certificate hash={key} NOT FOUND.");
            AppError::NotFound("".to_string())
        })
}

/// HTTP: return the list of certificates
pub async fn ctx_snapshots(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let certificates = app_state.get_ctx_snapshots().await?;

    Ok(certificates)
}

/// HTTP: return a cardano transaction snapshot identified by its hash.
pub async fn ctx_snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_ctx_snapshot(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("ctx snapshot hash={key} NOT FOUND.");
            AppError::NotFound("".to_string())
        })
}

#[derive(serde::Deserialize, Default)]
pub struct CardanoTransactionProofQueryParams {
    transaction_hashes: String,
}

/// HTTP: return a cardano transaction proof identified by a transaction hash.
pub async fn ctx_proof(
    params: Option<Query<CardanoTransactionProofQueryParams>>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;
    let Query(params) = params.unwrap_or_default();

    app_state
        .get_ctx_proofs(&params.transaction_hashes)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!(
                "ctx proof ctx_hash={} NOT FOUND.",
                params.transaction_hashes
            );
            AppError::NotFound("".to_string())
        })
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
    use crate::{default_values, shared_state::AppState};

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
        let digest = Path(default_values::snapshot_digests()[0].to_string());

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
        let hash = Path(default_values::certificate_hashes()[0].to_string());

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
        let hash = Path(default_values::msd_hashes()[0].to_string());

        let response = msd(hash, state)
            .await
            .expect("The handler was expected to succeed since the msd's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_ctx_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = ctx_snapshot(hash, state).await.expect_err(
            "The handler was expected to fail since the ctx snapshot's hash does not exist.",
        );

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_ctx_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path(default_values::ctx_snapshot_hashes()[0].to_string());

        let response = ctx_snapshot(hash, state).await.expect(
            "The handler was expected to succeed since the ctx snapshot's hash does exist.",
        );

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn no_hash_ctx_proof() {
        let state: State<SharedState> = State(AppState::default().into());

        let error = ctx_proof(None, state)
            .await
            .expect_err("The handler was expected to fail since no transaction hash was provided.");

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn invalid_ctx_proof_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let transaction_hashes = "whatever".to_string();

        let error = ctx_proof(
            Some(Query(CardanoTransactionProofQueryParams {
                transaction_hashes,
            })),
            state,
        )
        .await
        .expect_err("The handler was expected to fail since the ctx proof's hash does not exist.");

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_ctx_proof_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let transaction_hashes = default_values::proof_transaction_hashes()[0].to_string();

        let response = ctx_proof(
            Some(Query(CardanoTransactionProofQueryParams {
                transaction_hashes,
            })),
            state,
        )
        .await
        .expect("The handler was expected to succeed since the ctx proof's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }
}
