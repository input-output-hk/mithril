use axum::{
    body::Body,
    extract::State,
    http::{Response, StatusCode},
    response::IntoResponse,
    routing::get,
    Router,
};
use slog::{error, info, warn, Logger};
use std::sync::Arc;
use tokio::sync::oneshot::Receiver;

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

pub trait MetricsServiceExporter {
    fn export_metrics(&self) -> StdResult<String>;
}

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
                (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {:?}", e)).into_response()
            }
        }
    }
}

/// The MetricsServer is responsible for exposing the metrics of the signer.
pub struct MetricsServer<T: MetricsServiceExporter + Send + Sync> {
    server_port: u16,
    server_ip: String,
    metrics_service: Arc<T>,
    logger: Logger,
}

struct RouterState<T: MetricsServiceExporter + Send + Sync> {
    metrics_service: Arc<T>,
    logger: Logger,
}

impl<T: MetricsServiceExporter + Send + Sync + 'static> MetricsServer<T> {
    /// Create a new MetricsServer instance.
    pub fn new(server_ip: &str, server_port: u16, metrics_service: Arc<T>, logger: Logger) -> Self {
        Self {
            server_port,
            server_ip: server_ip.to_string(),
            metrics_service,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Metrics server endpoint.
    pub fn endpoint(&self) -> String {
        format!("http://{}:{}", self.server_ip, self.server_port)
    }

    /// Serve the metrics on an HTTP server.
    pub async fn start(&self, shutdown_rx: Receiver<()>) -> StdResult<()> {
        info!(
            self.logger,
            "starting HTTP server for metrics on port {}", self.server_port
        );

        let router_state = Arc::new(RouterState {
            metrics_service: self.metrics_service.clone(),
            logger: self.logger.clone(),
        });
        let app = Router::new()
            .route(
                "/metrics",
                get(|State(state): State<Arc<RouterState<T>>>| async move {
                    state.metrics_service.export_metrics().map_err(|e| {
                        error!(state.logger, "error exporting metrics"; "error" => ?e);
                        MetricsServerError::Internal(e)
                    })
                }),
            )
            .with_state(router_state);
        let listener =
            tokio::net::TcpListener::bind(format!("{}:{}", self.server_ip, self.server_port))
                .await?;

        let serve_logger = self.logger.clone();
        axum::serve(listener, app)
            .with_graceful_shutdown(async move {
                shutdown_rx.await.ok();
                warn!(
                    serve_logger,
                    "shutting down HTTP server after receiving signal"
                );
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

    use crate::test_tools::TestLogger;

    use super::*;

    pub struct PseudoMetricsService {}

    impl PseudoMetricsService {
        pub fn new() -> Self {
            Self {}
        }
    }

    impl MetricsServiceExporter for PseudoMetricsService {
        fn export_metrics(&self) -> StdResult<String> {
            Ok("pseudo metrics".to_string())
        }
    }

    #[tokio::test]
    async fn test_metrics_server() {
        let logger = TestLogger::stdout();
        let (shutdown_tx, shutdown_rx) = oneshot::channel();
        let metrics_service = Arc::new(PseudoMetricsService::new());
        let metrics_server = Arc::new(MetricsServer::new(
            "0.0.0.0",
            9090,
            metrics_service.clone(),
            logger,
        ));
        let metrics_server_endpoint = metrics_server.endpoint();

        let exported_metrics_test = tokio::spawn(async move {
            // Yield to make sure the web server starts first.
            yield_now().await;

            let response = reqwest::get(format!("{metrics_server_endpoint}/metrics"))
                .await
                .unwrap();

            assert_eq!(StatusCode::OK, response.status());
            assert_eq!("pseudo metrics", response.text().await.unwrap());
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
