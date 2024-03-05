use std::sync::Arc;

use axum::{
    body::Body,
    extract::State,
    http::{Response, StatusCode},
    response::IntoResponse,
    routing::get,
    Router,
};
use mithril_common::StdResult;
use slog_scope::{error, info, warn};
use tokio::sync::oneshot::Receiver;

use crate::MetricsService;

/// Metrics server errors
#[derive(Debug)]
pub enum MetricsServerError {
    /// Internal errors
    Internal(anyhow::Error),
}

/// Converts Metrics server error into axum response.
impl IntoResponse for MetricsServerError {
    fn into_response(self) -> Response<Body> {
        match self {
            Self::Internal(e) => {
                error!("{}", e);

                (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {:?}", e)).into_response()
            }
        }
    }
}

/// The MetricsServer is responsible for exposing the metrics of the signer.
pub struct MetricsServer {
    server_port: u16,
    server_ip: String,
    metrics_service: Arc<MetricsService>,
}

impl MetricsServer {
    /// Create a new MetricsServer instance.
    pub fn new(server_ip: &str, server_port: u16, metrics_service: Arc<MetricsService>) -> Self {
        Self {
            server_port,
            server_ip: server_ip.to_string(),
            metrics_service,
        }
    }

    /// Metrics server endpoint.
    pub fn endpoint(&self) -> String {
        format!("http://{}:{}", self.server_ip, self.server_port)
    }

    /// Serve the metrics on a HTTP server.
    pub async fn start(&self, shutdown_rx: Receiver<()>) -> StdResult<()> {
        info!(
            "MetricsServer: starting HTTP server for metrics on port {}",
            self.server_port
        );
        let app = Router::new()
            .route(
                "/metrics",
                get(|State(state): State<Arc<MetricsService>>| async move {
                    state.export_metrics().map_err(MetricsServerError::Internal)
                }),
            )
            .with_state(self.metrics_service.clone());
        let listener =
            tokio::net::TcpListener::bind(format!("{}:{}", self.server_ip, self.server_port))
                .await?;
        axum::serve(listener, app)
            .with_graceful_shutdown(async {
                warn!("MetricsServer: shutting down HTTP server after receiving signal");
                shutdown_rx.await.ok();
            })
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use reqwest::StatusCode;
    use std::time::Duration;
    use tokio::{sync::oneshot, task::yield_now, time::sleep};

    use super::*;

    #[tokio::test]
    async fn test_metrics_server() {
        let (shutdown_tx, shutdown_rx) = oneshot::channel();
        let metrics_service = Arc::new(MetricsService::new().unwrap());
        let metrics_server = Arc::new(MetricsServer::new("0.0.0.0", 9090, metrics_service.clone()));
        let metrics_server_endpoint = metrics_server.endpoint();

        let exported_metrics_test = tokio::spawn(async move {
            // Yield to make sure the web server starts first.
            yield_now().await;

            let response = reqwest::get(format!("{metrics_server_endpoint}/metrics"))
                .await
                .unwrap();

            assert_eq!(StatusCode::OK, response.status());
            assert_ne!("", response.text().await.unwrap());
        });

        tokio::select!(
            res =  metrics_server.start(shutdown_rx)  => Err(anyhow!("Metrics server exited with value '{res:?}'")),
            _res = sleep(Duration::from_secs(1)) => Err(anyhow!("Timeout: The test should have already completed.")),
            res = exported_metrics_test => res.map_err(|e| e.into()),
        )
        .unwrap();

        shutdown_tx.send(()).unwrap();
    }
}
