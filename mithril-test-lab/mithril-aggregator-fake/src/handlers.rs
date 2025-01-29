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
        .route(
            "/artifact/mithril-stake-distributions",
            get(mithril_stake_distributions),
        )
        .route(
            "/artifact/mithril-stake-distribution/{digest}",
            get(mithril_stake_distribution),
        )
        .route("/artifact/snapshot/{digest}", get(snapshot))
        .route(
            "/artifact/cardano-transactions",
            get(cardano_transaction_snapshots),
        )
        .route(
            "/artifact/cardano-transaction/{hash}",
            get(cardano_transaction_snapshot),
        )
        .route(
            "/artifact/cardano-stake-distributions",
            get(cardano_stake_distributions),
        )
        .route(
            "/artifact/cardano-stake-distribution/{hash}",
            get(cardano_stake_distribution),
        )
        .route(
            "/artifact/cardano-stake-distribution/epoch/{epoch}",
            get(cardano_stake_distribution_by_epoch),
        )
        .route(
            "/artifact/cardano-database",
            get(cardano_database_snapshots),
        )
        .route(
            "/artifact/cardano-database/{hash}",
            get(cardano_database_snapshot),
        )
        .route("/proof/cardano-transaction", get(cardano_transaction_proof))
        .route("/certificates", get(certificates))
        .route("/certificate/{hash}", get(certificate))
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
            AppError::NotFound
        })
}

/// HTTP: return the list of snapshots.
pub async fn snapshots(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let snapshots = app_state.get_snapshots().await?;

    Ok(snapshots)
}

/// HTTP: return the list of mithril stake distributions.
pub async fn mithril_stake_distributions(
    State(state): State<SharedState>,
) -> Result<String, AppError> {
    let app_state = state.read().await;
    let mithril_stake_distributions = app_state.get_mithril_stake_distributions().await?;

    Ok(mithril_stake_distributions)
}

/// HTTP: return a mithril stake distribution identified by its hash.
pub async fn mithril_stake_distribution(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_mithril_stake_distribution(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("mithril stake distribution epoch={key} NOT FOUND.");
            AppError::NotFound
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
            AppError::NotFound
        })
}

/// HTTP: return the list of Cardano transactions snapshots
pub async fn cardano_transaction_snapshots(
    State(state): State<SharedState>,
) -> Result<String, AppError> {
    let app_state = state.read().await;
    let certificates = app_state.get_cardano_transaction_snapshots().await?;

    Ok(certificates)
}

/// HTTP: return a cardano transaction snapshot identified by its hash.
pub async fn cardano_transaction_snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_cardano_transaction_snapshot(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("cardano transaction snapshot hash={key} NOT FOUND.");
            AppError::NotFound
        })
}

/// HTTP: return the list of cardano stake distributions.
pub async fn cardano_stake_distributions(
    State(state): State<SharedState>,
) -> Result<String, AppError> {
    let app_state = state.read().await;
    let cardano_stake_distributions = app_state.get_cardano_stake_distributions().await?;

    Ok(cardano_stake_distributions)
}

/// HTTP: return a cardano stake distribution identified by its hash.
pub async fn cardano_stake_distribution(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_cardano_stake_distribution(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("cardano stake distribution hash={key} NOT FOUND.");
            AppError::NotFound
        })
}

/// HTTP: return a cardano stake distribution identified by its epoch.
pub async fn cardano_stake_distribution_by_epoch(
    Path(epoch): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    #[derive(Debug, serde::Deserialize)]
    struct TmpCardanoStakeDistributionData {
        hash: String,
        epoch: u64,
    }

    let app_state = state.read().await;

    let cardano_stake_distributions = app_state.get_cardano_stake_distributions().await?;
    let cardano_stake_distributions: Vec<TmpCardanoStakeDistributionData> =
        serde_json::from_str(&cardano_stake_distributions)?;

    // Find the cardano stake distribution hash corresponding to the epoch
    let hash = cardano_stake_distributions
        .into_iter()
        .find(|csd| csd.epoch.to_string() == epoch)
        .map(|csd| csd.hash)
        .ok_or_else(|| {
            debug!("No cardano stake distribution found for epoch={epoch}.");
            AppError::NotFound
        })?;

    app_state
        .get_cardano_stake_distribution(&hash)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("cardano stake distribution hash={hash} NOT FOUND.");
            AppError::NotFound
        })
}

/// HTTP: return the list of cardano database snapshots.
pub async fn cardano_database_snapshots(
    State(state): State<SharedState>,
) -> Result<String, AppError> {
    let app_state = state.read().await;
    let cardano_database_snapshots = app_state.get_cardano_database_snapshots().await?;

    Ok(cardano_database_snapshots)
}

/// HTTP: return a cardano database snapshot identified by its hash.
pub async fn cardano_database_snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_cardano_database_snapshot(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!("cardano database hash={key} NOT FOUND.");
            AppError::NotFound
        })
}

#[derive(serde::Deserialize, Default)]
pub struct CardanoTransactionProofQueryParams {
    transaction_hashes: String,
}

/// HTTP: return a cardano transaction proof identified by a transaction hash.
pub async fn cardano_transaction_proof(
    Query(params): Query<CardanoTransactionProofQueryParams>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_cardano_transaction_proofs(&params.transaction_hashes)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| {
            debug!(
                "cardano transaction proof ctx_hash={} NOT FOUND.",
                params.transaction_hashes
            );
            AppError::NotFound
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

        assert!(matches!(error, AppError::NotFound));
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
    async fn invalid_mithril_stake_distribution_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = mithril_stake_distribution(hash, state)
            .await
            .expect_err("The handler was expected to fail since the msd's hash does not exist.");

        assert!(matches!(error, AppError::NotFound));
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

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn existing_mithril_stake_distribution_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path(default_values::mithril_stake_distribution_hashes()[0].to_string());

        let response = mithril_stake_distribution(hash, state)
            .await
            .expect("The handler was expected to succeed since the msd's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_cardano_transaction_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = cardano_transaction_snapshot(hash, state).await.expect_err(
            "The handler was expected to fail since the cardano transaction snapshot's hash does not exist.",
        );

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn existing_cardano_transaction_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path(default_values::cardano_transaction_snapshot_hashes()[0].to_string());

        let response = cardano_transaction_snapshot(hash, state).await.expect(
            "The handler was expected to succeed since the cardano transaction snapshot's hash does exist.",
        );

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn no_hash_cardano_transaction_proof() {
        let state: State<SharedState> = State(AppState::default().into());
        let transaction_hashes = "".to_string();

        let error = cardano_transaction_proof(
            Query(CardanoTransactionProofQueryParams { transaction_hashes }),
            state,
        )
        .await
        .expect_err("The handler was expected to fail since no transaction hash was provided.");

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn invalid_cardano_transaction_proof_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let transaction_hashes = "whatever".to_string();

        let error = cardano_transaction_proof(
            Query(CardanoTransactionProofQueryParams { transaction_hashes }),
            state,
        )
        .await
        .expect_err("The handler was expected to fail since the cardano transaction proof's hash does not exist.");

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn existing_cardano_transaction_proof_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let transaction_hashes = default_values::proof_transaction_hashes()[0].to_string();

        let response = cardano_transaction_proof(
            Query(CardanoTransactionProofQueryParams { transaction_hashes }),
            state,
        )
        .await
        .expect("The handler was expected to succeed since the cardano transaction proof's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn existing_cardano_stake_distribution_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path(default_values::cardano_stake_distribution_hashes()[0].to_string());

        let response = cardano_stake_distribution(hash, state)
            .await
            .expect("The handler was expected to succeed since the cardano stake distribution's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_cardano_stake_distribution_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = cardano_stake_distribution(hash, state)
            .await
            .expect_err("The handler was expected to fail since the cardano stake distribution's hash does not exist.");

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn existing_cardano_stake_distribution_epoch() {
        let state: State<SharedState> = State(AppState::default().into());
        let epoch = Path(default_values::cardano_stake_distribution_epochs()[0].to_string());

        let response = cardano_stake_distribution_by_epoch(epoch, state)
            .await
            .expect("The handler was expected to succeed since the cardano stake distribution's epoch does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_cardano_stake_distribution_epoch() {
        let state: State<SharedState> = State(AppState::default().into());
        let epoch = Path(u64::MAX.to_string());

        let error = cardano_stake_distribution_by_epoch(epoch, state)
            .await
            .expect_err("The handler was expected to fail since the cardano stake distribution's epoch does not exist.");

        assert!(matches!(error, AppError::NotFound));
    }

    #[tokio::test]
    async fn existing_cardano_database_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path(default_values::cardano_database_snapshot_hashes()[0].to_string());

        let response = cardano_database_snapshot(hash, state)
            .await
            .expect("The handler was expected to succeed since the cardano database snapshot's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_cardano_database_snapshot_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = cardano_database_snapshot(hash, state)
            .await
            .expect_err("The handler was expected to fail since the cardano database snapshot's hash does not exist.");

        assert!(matches!(error, AppError::NotFound));
    }
}
