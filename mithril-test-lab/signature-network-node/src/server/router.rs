use std::convert::Infallible;

use warp::{Filter, Reply};

use crate::entities::RouterDependencies;

/// Routes
pub fn routes(
    dependencies: RouterDependencies,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    let logger = slog_scope::logger().new(slog::o!("src" => "⇄ http server"));
    warp::any().and(pull_signatures(dependencies, logger))
}

/// GET /pull-signatures
fn pull_signatures(
    dependencies: RouterDependencies,
    logger: slog::Logger,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    warp::path!("pull-signatures")
        .and(warp::get())
        .and(with_dependency(&dependencies, |d| {
            d.available_signatures_registrations
        }))
        .and(warp::any().map(move || logger.clone()))
        .and_then(handlers::pull_signatures)
}

pub fn with_dependency<D, F>(
    dependencies: &RouterDependencies,
    extract: F,
) -> impl Filter<Extract = (D,), Error = Infallible> + Clone
where
    D: Clone + Send,
    F: Fn(RouterDependencies) -> D + Clone + Send,
{
    let dep = extract(dependencies.clone());
    warp::any().map(move || dep.clone())
}

mod handlers {
    use std::convert::Infallible;
    use std::sync::Arc;

    use slog::debug;
    use tokio::sync::Mutex;
    use warp::hyper::StatusCode;

    use mithril_common::messages::RegisterSignatureMessage;

    use crate::server::reply;

    pub async fn pull_signatures(
        available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
        logger: slog::Logger,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "/pull-signatures");
        let mut signatures = available_signatures_registrations.lock().await;
        let sig = std::mem::take(&mut *signatures);

        Ok(reply::json(&sig, StatusCode::OK))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use slog::Logger;
    use tokio::sync::Mutex;

    use mithril_common::messages::RegisterSignatureMessage;
    use mithril_common::socket_client::HttpUnixSocketClient;
    use mithril_common::test_utils::test_http_server::test_http_server_with_unix_socket;
    use mithril_common::test_utils::TempDir;

    use super::*;

    #[tokio::test]
    async fn pull_signatures_when_empty() {
        let dir = TempDir::create(
            "signature-network-node-router",
            "pull_signatures_when_empty",
        );
        let socket_path = dir.join("test.sock");
        let dependencies = RouterDependencies {
            available_signatures_registrations: Arc::new(Mutex::new(Vec::new())),
        };
        let _server = test_http_server_with_unix_socket(
            pull_signatures(
                dependencies.clone(),
                Logger::root(slog::Discard, slog::o!()),
            ),
            &socket_path,
        );

        let client = HttpUnixSocketClient::new(&socket_path);
        let signatures: Vec<RegisterSignatureMessage> = client.read("pull-signatures").unwrap();

        assert_eq!(Vec::<RegisterSignatureMessage>::new(), signatures);
    }

    #[tokio::test]
    async fn pull_available_signatures() {
        let dir = TempDir::create("signature-network-node-router", "pull_available_signatures");
        let socket_path = dir.join("test.sock");
        let dependencies = RouterDependencies {
            available_signatures_registrations: Arc::new(Mutex::new(vec![
                RegisterSignatureMessage::dummy(),
            ])),
        };
        let _server = test_http_server_with_unix_socket(
            pull_signatures(
                dependencies.clone(),
                Logger::root(slog::Discard, slog::o!()),
            ),
            &socket_path,
        );

        let client = HttpUnixSocketClient::new(&socket_path);
        let signatures: Vec<RegisterSignatureMessage> = client.read("pull-signatures").unwrap();

        assert_eq!(vec![RegisterSignatureMessage::dummy()], signatures);

        // Pulling signatures should clear the available signatures list, so the next pull
        // should return an empty list
        let signatures: Vec<RegisterSignatureMessage> = client.read("pull-signatures").unwrap();

        assert_eq!(Vec::<RegisterSignatureMessage>::new(), signatures);
    }
}
