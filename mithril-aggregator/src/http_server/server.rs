use crate::http_server::routes::router;
use crate::DependencyManager;
use slog_scope::info;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;

pub const SERVER_BASE_PATH: &str = "aggregator";

/// Server
pub struct Server {
    ip: IpAddr,
    port: u16,
    dependency_manager: Arc<DependencyManager>,
}

impl Server {
    /// Server factory
    pub fn new(ip: String, port: u16, dependency_manager: Arc<DependencyManager>) -> Self {
        Self {
            ip: ip.parse::<IpAddr>().unwrap(),
            port,
            dependency_manager,
        }
    }

    /// Start
    pub async fn start(&self, shutdown_signal: impl Future<Output = ()> + Send + 'static) {
        info!("Start Aggregator Http Server");
        let routes = router::routes(self.dependency_manager.clone());
        let (_, server) =
            warp::serve(routes).bind_with_graceful_shutdown((self.ip, self.port), shutdown_signal);
        tokio::spawn(server).await.unwrap();
    }
}
