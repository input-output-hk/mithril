use crate::DependencyManager;
use slog_scope::info;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;
use warp::{http::Method, Filter};

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

mod router {
    use super::*;
    use crate::http_server::{
        certificate_routes, signatures_routes, signer_routes, snapshot_routes,
    };

    /// Routes
    pub fn routes(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(warp::path(SERVER_BASE_PATH)).and(
            certificate_routes::routes(dependency_manager.clone())
                .or(snapshot_routes::routes(dependency_manager.clone()))
                .or(signer_routes::routes(dependency_manager.clone()))
                .or(signatures_routes::routes(dependency_manager))
                .with(cors),
        )
    }
}
