use std::future::IntoFuture;

use anyhow::{anyhow, Context};
use axum::{
    middleware::{self},
    routing::{get, post},
    Router,
};
use futures::stream::StreamExt;
use signal_hook::consts::*;
use signal_hook_tokio::Signals;
use tower_http::{
    cors::CorsLayer,
    trace::{DefaultMakeSpan, DefaultOnRequest, DefaultOnResponse, TraceLayer},
    LatencyUnit,
};
use tracing::{debug, info, trace, warn, Level};

use crate::shared_state::{AppState, SharedState};
use crate::{handlers, CliArguments, StdResult};

/// Structure to hold signal listener
pub struct OsSignalHandler;

impl OsSignalHandler {
    /// define what to do when a signal is caught
    pub async fn handle_signal(mut signals: Signals) {
        while let Some(signal) = signals.next().await {
            match signal {
                SIGTERM | SIGINT | SIGQUIT => {
                    warn!("Signal caught: {signal}, exiting…");

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

/// Application controller
pub struct Application;

impl Application {
    /// main async runner
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
            .with_state(shared_state)
            .layer(CorsLayer::permissive())
            .layer(middleware::from_fn(handlers::set_json_app_header))
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

#[cfg(test)]
mod tests {
    use http::StatusCode;
    use serde_json::Value::Null;
    use std::time::Duration;
    use tokio::{
        task::{yield_now, JoinHandle},
        time::sleep,
    };

    use mithril_common::test_utils::apispec::APISpec;

    use crate::CliArguments;

    use super::*;

    const BASE_URL: &str = "http://127.0.0.1:PORT/aggregator";

    // tester
    async fn test(fn_test: JoinHandle<StdResult<()>>, port: u16) {
        let params = CliArguments {
            data_directory: None,
            verbose: 0,
            tcp_port: port,
            ip_address: "127.0.0.1".to_owned(),
            quiet: false,
        };

        tokio::select!(
            res =  Application::run(params)  => Err(anyhow!("Server web exited with value '{res:?}' !")),
            _res = sleep(Duration::from_secs(1)) => Err(anyhow!("Timeout: The test took too long to respond.")),
            res = fn_test => res.map_err(|e| e.into()),
        )
        .unwrap()
        .unwrap();
    }

    #[tokio::test]
    async fn get_certificates() {
        const PORT: u16 = 3001;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/certificates";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_snapshots() {
        const PORT: u16 = 3002;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/snapshots";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_msds() {
        const PORT: u16 = 3003;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/mithril-stake-distributions";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_certificate() {
        const PORT: u16 = 3004;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/certificate/{certificate_hash}";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let certificate_hash =
                "74ad5da3825aea1c9c2323a42cdcb4abebeee0424fec41885973f57f6520a164";
            let response = reqwest::get(&format!(
                "{url}{}",
                path.replace("{certificate_hash}", certificate_hash)
            ))
            .await
            .unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_certificate() {
        const PORT: u16 = 3005;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/certificate/whatever";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::NOT_FOUND, response.status());

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_snapshot() {
        const PORT: u16 = 3006;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/snapshot/{digest}";
            let digest = "83d3b70729cf0d2c27f91c49aba6ec7e90210511d9e431c6c4c7db8996386072";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{}", path.replace("{digest}", digest)))
                .await
                .unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_snapshot() {
        const PORT: u16 = 3007;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/snapshot/{digest}";
            let digest = "whatever";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{}", path.replace("{digest}", digest)))
                .await
                .unwrap();

            assert_eq!(StatusCode::NOT_FOUND, response.status());

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_msd() {
        const PORT: u16 = 3008;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/mithril-stake-distribution/{hash}";
            let hash = "11b6f0165d431ba6a9906c8f8ffab317e10104a06c986517165fc7766cc22dbe";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{}", path.replace("{hash}", hash)))
                .await
                .unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_msd() {
        const PORT: u16 = 3009;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/artifact/mithril_stake_distribution/whatever";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::NOT_FOUND, response.status());

            Ok(())
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_epoch_settings() {
        const PORT: u16 = 3010;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going
            // on.
            yield_now().await;

            let path = "/epoch-settings";
            let url = BASE_URL.replace("PORT", &PORT.to_string());
            let response = reqwest::get(&format!("{url}{path}")).await.unwrap();

            assert_eq!(StatusCode::OK, response.status());

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &http::Response::new(response.bytes().await.unwrap()),
            );

            Ok(())
        });

        test(task, PORT).await;
    }
}
