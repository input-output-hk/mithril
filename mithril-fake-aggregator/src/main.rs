use std::{future::IntoFuture, path::PathBuf};

use anyhow::{anyhow, Context};
use axum::{routing::get, Router};
use clap::Parser;
use futures::stream::StreamExt;
use signal_hook::consts::*;
use signal_hook_tokio::Signals;
use tower_http::{
    trace::{DefaultMakeSpan, DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::{debug, error, info, trace, Level};

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

#[tracing::instrument]
async fn epoch_settings() -> String {
    r#"{"epoch":112,"protocol":{"k":5,"m":100,"phi_f":0.65},"next_protocol":{"k":5,"m":100,"phi_f":0.65}}"#.to_string()
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

    trace!("configuring router…");
    let app = Router::new()
        .route("/epoch-settings", get(epoch_settings))
        .layer(
            TraceLayer::new_for_http()
                .make_span_with(DefaultMakeSpan::new().include_headers(true))
                .on_request(DefaultOnRequest::new().level(Level::DEBUG))
                .on_response(
                    DefaultOnResponse::new()
                        .include_headers(true)
                        .level(Level::INFO)
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
