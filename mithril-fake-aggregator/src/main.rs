mod shared_state;

use std::{
    future::IntoFuture,
    path::{Path as StdPath, PathBuf},
};

use anyhow::{anyhow, Context};
use axum::{
    body::Body,
    extract::{Path, State},
    http::{Response, StatusCode},
    response::IntoResponse,
    routing::get,
    Json, Router,
};
use clap::Parser;
use futures::stream::StreamExt;
use signal_hook::consts::*;
use signal_hook_tokio::Signals;
use tower_http::{
    trace::{DefaultMakeSpan, DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::{debug, error, info, trace, Level};

use crate::shared_state::{AppState, SharedState};

type StdResult<T> = anyhow::Result<T>;

/// Possible command line options and arguments
#[derive(Debug, Parser)]
#[command(version)]
pub struct CliArguments {
    /// Directory where the response files are located.
    #[arg(short, long, default_value = "data")]
    data_directory: PathBuf,

    /// Verbose mode (-q, -v, -vv, -vvv, etc)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// TCP port to listen on
    #[arg(short = 'p', long, default_value_t = 80)]
    tcp_port: u16,

    /// IP Address to bind server to
    #[arg(short, long, default_value = "127.0.0.1")]
    ip_address: String,
}

impl CliArguments {
    pub fn get_verbosity_level(&self) -> Level {
        match self.verbose {
            0 => Level::ERROR,
            1 => Level::WARN,
            2 => Level::INFO,
            3 => Level::DEBUG,
            _ => Level::TRACE,
        }
    }
}

/// error that wraps `anyhow::Error`.
pub struct AppError(anyhow::Error);

/// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AppError {
    fn into_response(self) -> Response<Body> {
        error!("{}", self.0);

        (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("Error: {:?}", self.0),
        )
            .into_response()
    }
}

// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}

pub async fn epoch_settings_handler(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let epoch_settings = app_state.get_epoch_settings().await?;

    Ok(epoch_settings.into())
}

pub async fn snapshot_handler(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> StdResult<Json<String>> {
    todo!()
}

pub async fn snapshots_handler(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let snapshots = app_state.get_snapshots().await?;

    Ok(snapshots)
}

pub async fn msds_handler(State(state): State<SharedState>) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn msd_handler(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn certificates_handler(
    State(state): State<SharedState>,
) -> Result<Json<String>, AppError> {
    todo!()
}

pub async fn certificate_handler(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Json<String>, AppError> {
    todo!()
}

pub struct OsSignalHandler;

impl OsSignalHandler {
    pub async fn handle_signal(mut signals: Signals) {
        while let Some(signal) = signals.next().await {
            match signal {
                SIGTERM | SIGINT | SIGQUIT => {
                    tracing::warn!("Signal caught: {signal}");

                    break;
                }
                _ => unreachable!(),
            }
        }
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let params = CliArguments::parse();
    tracing_subscriber::fmt()
        .with_max_level(params.get_verbosity_level())
        .init();
    info!(
        "starting Fake Aggregator version {}",
        env!("CARGO_PKG_VERSION")
    );

    trace!("setting up signal hook…");
    // supported signals
    let signals = Signals::new(&[SIGTERM, SIGINT, SIGQUIT])?;

    // launch signal detector
    let signal_handler = signals.handle();

    trace!("setting up shared state…");
    let shared_state: SharedState = AppState::default().into();

    trace!("configuring router…");
    let app = Router::new()
        .route("/aggregator/epoch-settings", get(epoch_settings_handler))
        .route("/aggregator/artifact/snapshots", get(snapshots_handler))
        .route(
            "/aggregator/artifact/mithril-stake-distributions",
            get(msds_handler),
        )
        .route(
            "/aggregator/artifact/mithril-stake-distribution/:digest",
            get(msd_handler),
        )
        .with_state(shared_state.clone())
        .layer(
            TraceLayer::new_for_http()
                .make_span_with(
                    DefaultMakeSpan::new()
                        .include_headers(true)
                        .level(Level::DEBUG),
                )
                .on_request(DefaultOnRequest::new().level(Level::DEBUG))
                .on_response(
                    DefaultOnResponse::new()
                        .level(Level::INFO)
                        .include_headers(true)
                        .latency_unit(LatencyUnit::Micros),
                ),
        );
    let listener = {
        let connection_string = format!("{}:{}", params.ip_address, params.tcp_port);
        debug!("binding on {connection_string}");
        tokio::net::TcpListener::bind(&connection_string)
            .await
            .with_context(|| format!("Could not listen on '{}'.", connection_string))?
    };

    trace!("starting server…");
    let result = tokio::select!(
        res = axum::serve(listener, app).into_future() => res.map_err(|e| anyhow!(e)),
        _res = OsSignalHandler::handle_signal(signals) => Ok(()),
    );

    trace!("closing signal handler…");
    signal_handler.close();

    match &result {
        Err(e) => error!("{e}"),
        Ok(_) => trace!("terminated!"),
    };

    result
}
