use axum::{
    Router,
    body::Body,
    extract::State,
    http::{Response, StatusCode},
    response::IntoResponse,
    routing::get,
};
use slog::{Logger, error, info, warn};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::sync::watch::Receiver;

use mithril_common::StdResult;
use mithril_common::logging::LoggerExtensions;

/// Metrics service exporter gives the possibility of exporting metrics.
pub trait MetricsServiceExporter: Send + Sync {
    /// Export metrics.
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
                (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {e:?}")).into_response()
            }
        }
    }
}

/// The MetricsServer is responsible for exposing the metrics of the signer.
pub struct MetricsServer {
    tcp_listener: TcpListener,
    axum_app: Router,
    address: SocketAddr,
    logger: Logger,
}

/// Builder for the [MetricsServer].
pub struct MetricsServerBuilder<T: MetricsServiceExporter> {
    server_port: u16,
    server_ip: String,
    metrics_service: Arc<T>,
    logger: Logger,
}

struct RouterState<T: MetricsServiceExporter> {
    metrics_service: Arc<T>,
    logger: Logger,
}

impl MetricsServer {
    /// Start building a new `MetricsServer` instance.
    pub fn build<T: MetricsServiceExporter + 'static>(
        server_ip: &str,
        server_port: u16,
        metrics_service: Arc<T>,
        logger: Logger,
    ) -> MetricsServerBuilder<T> {
        MetricsServerBuilder::new(server_ip, server_port, metrics_service, logger)
    }

    /// Metrics server endpoint.
    pub fn address(&self) -> SocketAddr {
        self.address
    }

    /// Serve the metrics server.
    pub async fn serve(self, shutdown_rx: Receiver<()>) -> StdResult<()> {
        let serve_logger = self.logger;
        let mut shutdown_rx = shutdown_rx;
        axum::serve(self.tcp_listener, self.axum_app)
            .with_graceful_shutdown(async move {
                shutdown_rx.changed().await.ok();
                warn!(
                    serve_logger,
                    "shutting down HTTP server after receiving signal"
                );
            })
            .await?;

        Ok(())
    }
}

impl<T: MetricsServiceExporter + 'static> MetricsServerBuilder<T> {
    /// Create a new MetricsServer instance.
    pub fn new(server_ip: &str, server_port: u16, metrics_service: Arc<T>, logger: Logger) -> Self {
        Self {
            server_port,
            server_ip: server_ip.to_string(),
            metrics_service,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Prepare the metrics server
    pub async fn bind(self) -> StdResult<MetricsServer> {
        info!(
            self.logger,
            "Starting HTTP server for metrics on port {}", self.server_port
        );

        let router_state = Arc::new(RouterState {
            metrics_service: self.metrics_service,
            logger: self.logger.clone(),
        });
        let axum_app = Router::new()
            .route(
                "/metrics",
                get(|State(state): State<Arc<RouterState<T>>>| async move {
                    state.metrics_service.export_metrics().map_err(|e| {
                        error!(state.logger, "Error exporting metrics"; "error" => ?e);
                        MetricsServerError::Internal(e)
                    })
                }),
            )
            .with_state(router_state);
        let tcp_listener =
            TcpListener::bind(format!("{}:{}", self.server_ip, self.server_port)).await?;
        let address = tcp_listener.local_addr()?;

        Ok(MetricsServer {
            tcp_listener,
            axum_app,
            address,
            logger: self.logger,
        })
    }

    /// Prepare and serve the metrics server.
    pub async fn serve(self, shutdown_rx: Receiver<()>) -> StdResult<()> {
        let server = self.bind().await?;
        server.serve(shutdown_rx).await
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use reqwest::StatusCode;
    use std::time::Duration;
    use tokio::{sync::watch, task::yield_now, time::sleep};

    use crate::helper::test_tools::TestLogger;

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
        let (shutdown_tx, shutdown_rx) = watch::channel(());
        let metrics_service = Arc::new(PseudoMetricsService::new());
        let metrics_server = MetricsServer::build(
            "127.0.0.1",
            0, // Let the OS pick a free port.
            metrics_service.clone(),
            logger,
        )
        .bind()
        .await
        .unwrap();
        let metrics_server_address = metrics_server.address();

        let exported_metrics_test = tokio::spawn(async move {
            // Yield to make sure the web server starts first.
            yield_now().await;

            let response = reqwest::get(format!("http://{metrics_server_address}/metrics"))
                .await
                .unwrap();

            assert_eq!(StatusCode::OK, response.status());
            assert_eq!("pseudo metrics", response.text().await.unwrap());
        });

        let res = tokio::select!(
            res =  metrics_server.serve(shutdown_rx)  => Err(anyhow!("Metrics server exited with value '{res:?}'")),
            _res = sleep(Duration::from_secs(1)) => Err(anyhow!("Timeout: The test should have already completed.")),
            res = exported_metrics_test => res.map_err(|e| e.into()),
        );

        shutdown_tx.send(()).unwrap();
        res.unwrap();
    }
}
