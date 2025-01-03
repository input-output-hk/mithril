use std::future::IntoFuture;

use anyhow::{anyhow, Context};
use axum::Router;
use futures::stream::StreamExt;
use signal_hook::consts::*;
use signal_hook_tokio::Signals;
use tracing::{debug, info, trace, warn};

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
        let router = Router::new()
            .nest("/aggregator", handlers::aggregator_router().await)
            .with_state(shared_state);
        let listener = {
            let connection_string = format!("{}:{}", params.ip_address, params.tcp_port);
            debug!("binding on {connection_string}");
            tokio::net::TcpListener::bind(&connection_string)
                .await
                .with_context(|| format!("Could not listen on '{}'.", connection_string))?
        };

        trace!("starting server…");
        let result = tokio::select!(
            res = axum::serve(listener, router).into_future() => res.map_err(|e| anyhow!(e)),
            _res = OsSignalHandler::handle_signal(signals) => Ok(()),
        );

        trace!("closing signal handler…");
        signal_handler.close();

        result
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value::Null;
    use std::time::Duration;
    use tokio::{
        task::{yield_now, JoinHandle},
        time::sleep,
    };
    use warp::http::{Response, StatusCode};
    use warp::hyper::body::Bytes;

    use mithril_common::test_utils::apispec::APISpec;

    use crate::{default_values, CliArguments};

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

    async fn into_response(response: reqwest::Response) -> Response<Bytes> {
        Response::builder()
            .status(StatusCode::from_u16(response.status().into()).unwrap())
            .body(response.bytes().await.unwrap())
            .unwrap()
    }

    async fn http_request(port: u16, path: &str) -> Response<Bytes> {
        let url = BASE_URL.replace("PORT", &port.to_string());
        let response = reqwest::get(&format!("{url}{path}")).await.unwrap();
        into_response(response).await
    }

    fn get_spec_files() -> Vec<String> {
        APISpec::get_all_spec_files_from("../..")
    }

    #[tokio::test]
    async fn get_certificates() {
        const PORT: u16 = 3001;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/certificates";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_snapshots() {
        const PORT: u16 = 3002;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/snapshots";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_msds() {
        const PORT: u16 = 3003;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/mithril-stake-distributions";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_certificate() {
        const PORT: u16 = 3004;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/certificate/{certificate_hash}";
            let certificate_hash = default_values::certificate_hashes()[0];
            let response =
                http_request(PORT, &path.replace("{certificate_hash}", certificate_hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_certificate() {
        const PORT: u16 = 3005;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/certificate/{certificate_hash}";
            let response =
                http_request(PORT, &path.replace("{certificate_hash}", "whatever")).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_snapshot() {
        const PORT: u16 = 3006;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/snapshot/{digest}";
            let digest = default_values::snapshot_digests()[0];
            let response = http_request(PORT, path.replace("{digest}", digest).as_str()).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_snapshot() {
        const PORT: u16 = 3007;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/snapshot/{digest}";
            let digest = "whatever";
            let response = http_request(PORT, &path.replace("{digest}", digest)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_msd() {
        const PORT: u16 = 3008;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/mithril-stake-distribution/{hash}";
            let hash = default_values::msd_hashes()[0];
            let response = http_request(PORT, &path.replace("{hash}", hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_msd() {
        const PORT: u16 = 3009;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/mithril-stake-distribution/{hash}";
            let response = http_request(PORT, &path.replace("{hash}", "whatever")).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_epoch_settings() {
        const PORT: u16 = 3010;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/epoch-settings";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_ctx_snapshot() {
        const PORT: u16 = 3011;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-transaction/{hash}";
            let hash = default_values::ctx_snapshot_hashes()[0];
            let response = http_request(PORT, &path.replace("{hash}", hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_ctx_snapshot() {
        const PORT: u16 = 3012;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-transaction/{hash}";
            let hash = "whatever";
            let response = http_request(PORT, &path.replace("{hash}", hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_ctx_proof() {
        const PORT: u16 = 3013;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/proof/cardano-transaction?transaction_hashes={hash}";
            let hash = default_values::proof_transaction_hashes()[0];
            let response = http_request(PORT, &path.replace("{hash}", hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_ctx_proof() {
        const PORT: u16 = 3014;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/proof/cardano-transaction?transaction_hashes=";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution() {
        const PORT: u16 = 3015;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-stake-distribution/{hash}";
            let hash = default_values::csd_hashes()[0];
            let response = http_request(PORT, &path.replace("{hash}", hash)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_cardano_stake_distribution() {
        const PORT: u16 = 3016;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-stake-distribution/{hash}";
            let response = http_request(PORT, &path.replace("{hash}", "whatever")).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_cardano_stake_distributions() {
        const PORT: u16 = 3017;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-stake-distributions";
            let response = http_request(PORT, path).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution_by_epoch() {
        const PORT: u16 = 3018;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-stake-distribution/epoch/{epoch}";
            let epoch = default_values::csd_epochs()[0];
            let response = http_request(PORT, &path.replace("{epoch}", epoch)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::OK,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }

    #[tokio::test]
    async fn get_no_cardano_stake_distribution_by_epoch() {
        const PORT: u16 = 3019;
        let task = tokio::spawn(async move {
            // Yield back to Tokio's scheduler to ensure the web server is ready before going on.
            yield_now().await;

            let path = "/artifact/cardano-stake-distribution/epoch/{epoch}";
            let epoch = &u64::MAX.to_string();
            let response = http_request(PORT, &path.replace("{epoch}", epoch)).await;

            APISpec::verify_conformity(
                get_spec_files(),
                "GET",
                path,
                "application/json",
                &Null,
                &response,
                &StatusCode::NOT_FOUND,
            )
            .map_err(|e| anyhow!(e))
        });

        test(task, PORT).await;
    }
}
