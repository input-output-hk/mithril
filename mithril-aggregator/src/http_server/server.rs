use crate::http_server::routes::router;
use crate::DependencyManager;
use slog_scope::info;
use std::net::IpAddr;
use std::sync::Arc;

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
    pub async fn start(&self) -> impl warp::Future<Output = ()> + 'static {
        info!("Start Aggregator Http Server");
        let routes = router::routes(self.dependency_manager.clone());
        warp::serve(routes).bind((self.ip, self.port))
    }
}
