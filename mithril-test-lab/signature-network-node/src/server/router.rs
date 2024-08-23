use std::convert::Infallible;

use crate::entities::RouterDependencies;
use warp::{Filter, Reply};

/// Routes
pub fn routes(
    dependencies: RouterDependencies,
    parent_logger: &slog::Logger,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    let logger = parent_logger.new(slog::o!("src" => "⇄ http server"));
    warp::any()
        .and(pull_signatures(&dependencies, &logger))
        .or(register_signatures(&dependencies, &logger))
}

/// GET /pull-signatures
fn pull_signatures(
    dependencies: &RouterDependencies,
    logger: &slog::Logger,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    let logger = logger.clone();
    warp::path!("pull-signatures")
        .and(warp::get())
        .and(with_dependency(dependencies, |d| {
            d.available_signatures_registrations
        }))
        .and(warp::any().map(move || logger.clone()))
        .and_then(handlers::pull_signatures)
}

/// POST /register-signatures
fn register_signatures(
    dependencies: &RouterDependencies,
    logger: &slog::Logger,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    let logger = logger.clone();
    warp::path!("register-signatures")
        .and(warp::post())
        .and(warp::body::json())
        .and(with_dependency(dependencies, |d| {
            d.incoming_messages_sender
        }))
        .and(warp::any().map(move || logger.clone()))
        .and_then(handlers::register_signatures)
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
    use tokio::sync::{mpsc, Mutex};
    use warp::hyper::StatusCode;

    use mithril_common::messages::RegisterSignatureMessage;

    use crate::entities::Message;
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

    pub async fn register_signatures(
        signature: RegisterSignatureMessage,
        incoming_messages_sender: mpsc::Sender<Message>,
        logger: slog::Logger,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, "/register-signatures/{:?}", signature);

        match incoming_messages_sender
            .send(Message::MithrilRegisterSignature(signature))
            .await
        {
            Ok(()) => Ok(reply::empty(StatusCode::CREATED)),
            Err(err) => {
                debug!(logger, "Error sending message to channel: {:?}", err);
                Ok(reply::internal_server_error(
                    "Error sending message to channel".to_string(),
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mithril_common::messages::RegisterSignatureMessage;
    use mithril_common::socket_client::HttpUnixSocketClient;
    use mithril_common::test_utils::test_http_server::test_http_server_with_unix_socket;
    use mithril_common::test_utils::TempDir;
    use slog::Logger;
    use tokio::sync::mpsc::error::TryRecvError;
    use tokio::sync::{mpsc, Mutex};

    use crate::entities::Message;

    use super::*;

    fn discard_logs() -> Logger {
        Logger::root(slog::Discard, slog::o!())
    }

    #[tokio::test]
    async fn pull_signatures_when_empty() {
        let dir = TempDir::create(
            "signature-network-node-router",
            "pull_signatures_when_empty",
        );
        let socket_path = dir.join("test.sock");
        let dependencies = RouterDependencies {
            available_signatures_registrations: Arc::new(Mutex::new(Vec::new())),
            incoming_messages_sender: mpsc::channel(1).0,
        };
        let _server = test_http_server_with_unix_socket(
            pull_signatures(&dependencies, &discard_logs()),
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
            incoming_messages_sender: mpsc::channel(1).0,
        };
        let _server = test_http_server_with_unix_socket(
            pull_signatures(&dependencies, &discard_logs()),
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

    #[tokio::test]
    async fn register_signatures_push_them_to_channel() {
        let dir = TempDir::create(
            "signature-network-node-router",
            "register_signatures_push_them_to_channel",
        );
        let socket_path = dir.join("test.sock");
        let (tx, mut rx) = mpsc::channel(1);
        let dependencies = RouterDependencies {
            available_signatures_registrations: Arc::new(Mutex::new(vec![
                RegisterSignatureMessage::dummy(),
            ])),
            incoming_messages_sender: tx,
        };
        let _server = test_http_server_with_unix_socket(
            register_signatures(&dependencies, &discard_logs()),
            &socket_path,
        );

        let client = HttpUnixSocketClient::new(&socket_path);

        // No signatures should have been received yet
        assert_eq!(Err(TryRecvError::Empty), rx.try_recv());

        client
            .write("register-signatures", &RegisterSignatureMessage::dummy())
            .unwrap();

        assert_eq!(
            Ok(Message::MithrilRegisterSignature(
                RegisterSignatureMessage::dummy()
            )),
            rx.try_recv(),
        );
    }
}
