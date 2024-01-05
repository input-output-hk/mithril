mod handlers;
mod shared_state;

use std::{future::IntoFuture, path::PathBuf};

use anyhow::{anyhow, Context};
use axum::{
    body::Body,
    extract::Request,
    http::{HeaderValue, Response, StatusCode},
    middleware::{self, Next},
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use clap::Parser;
use futures::stream::StreamExt;
use signal_hook::consts::*;
use signal_hook_tokio::Signals;
use tower_http::{
    trace::{DefaultMakeSpan, DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::{debug, error, info, trace, warn, Level};

use crate::shared_state::{AppState, SharedState};

type StdResult<T> = anyhow::Result<T>;

/// Possible command line options and arguments
#[derive(Debug, Parser)]
#[command(version)]
pub struct CliArguments {
    /// Directory where the response files are located.
    #[arg(short, long)]
    data_directory: Option<PathBuf>,

    /// Verbosity level  (-v WARN, -vv INFO, -vvv DEBUG, etc)
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// TCP port to listen on
    #[arg(short = 'p', long, default_value_t = 80)]
    tcp_port: u16,

    /// IP Address to bind server to
    #[arg(short, long, default_value = "127.0.0.1")]
    ip_address: String,

    /// Quiet mode, no log will be emitted. Critical error messages will still pop on STDERR
    #[arg(short, long, default_value_t = false)]
    quiet: bool,
}

impl CliArguments {
    pub fn get_verbosity_level(&self) -> Option<Level> {
        if self.quiet {
            None
        } else {
            match self.verbose {
                0 => Some(Level::ERROR),
                1 => Some(Level::WARN),
                2 => Some(Level::INFO),
                3 => Some(Level::DEBUG),
                _ => Some(Level::TRACE),
            }
        }
    }
}

/// error that wraps `anyhow::Error`.
pub enum AppError {
    /// Catching anyhow errors
    Internal(anyhow::Error),

    /// Resource not found (specify what)
    NotFound(String),
}

/// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AppError {
    fn into_response(self) -> Response<Body> {
        match self {
            Self::Internal(e) => {
                error!("{}", e);

                (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {:?}", e)).into_response()
            }
            Self::NotFound(resource) => (
                StatusCode::NOT_FOUND,
                format!("resource '{resource}' not found"),
            )
                .into_response(),
        }
    }
}

/// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
/// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self::Internal(err.into())
    }
}

pub struct OsSignalHandler;

impl OsSignalHandler {
    pub async fn handle_signal(mut signals: Signals) {
        while let Some(signal) = signals.next().await {
            match signal {
                SIGTERM | SIGINT | SIGQUIT => {
                    warn!("Signal caught: {signal}, exiting.");

                    break;
                }
                SIGHUP => {
                    info!("SIGHUP signal caught.");
                }
                _ => unreachable!(),
            }
        }
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let params = CliArguments::parse();

    if let Some(level) = params.get_verbosity_level() {
        tracing_subscriber::fmt().with_max_level(level).init();
    }

    let result = Application::run(params).await;

    match &result {
        Err(e) => error!("{e}"),
        Ok(_) => debug!("soft terminated"),
    };

    result
}
struct Application;

impl Application {
    pub async fn run(params: CliArguments) -> StdResult<()> {
        info!(
            "starting Fake Aggregator version {}",
            env!("CARGO_PKG_VERSION")
        );

        trace!("setting up signal hook…");
        // supported signals
        let signals = Signals::new([SIGTERM, SIGINT, SIGQUIT, SIGHUP])?;

        // launch signal detector
        let signal_handler = signals.handle();

        trace!("setting up shared state…");
        let shared_state: SharedState = match params.data_directory {
            Some(directory) => {
                info!("Read data files from directory '{}'.", directory.display());
                AppState::from_directory(&directory)?.into()
            }
            None => {
                debug!("Using default data set.");
                AppState::default().into()
            }
        };

        trace!("configuring router…");
        let app = Router::new()
            .route("/aggregator/epoch-settings", get(handlers::epoch_settings))
            .route("/aggregator/artifact/snapshots", get(handlers::snapshots))
            .route(
                "/aggregator/artifact/mithril-stake-distributions",
                get(handlers::msds),
            )
            .route(
                "/aggregator/artifact/mithril-stake-distribution/:digest",
                get(handlers::msd),
            )
            .route(
                "/aggregator/artifact/snapshot/:digest",
                get(handlers::snapshot),
            )
            .route("/aggregator/certificates", get(handlers::certificates))
            .route("/aggregator/certificate/:hash", get(handlers::certificate))
            .route(
                "/aggregator/statistics/snapshot",
                post(handlers::statistics),
            )
            .with_state(shared_state.clone())
            .layer(middleware::from_fn(set_json_app_header))
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

        result
    }
}

async fn set_json_app_header(
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
mod tests {}
