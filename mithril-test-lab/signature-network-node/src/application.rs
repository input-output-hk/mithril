use slog::{crit, debug, info};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use tokio::net::UnixListener;
use tokio::signal::unix::{signal, SignalKind};
use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinSet;
use tokio_stream::wrappers::UnixListenerStream;

use mithril_common::messages::RegisterSignatureMessage;
use mithril_common::StdResult;

use crate::server::router;

pub struct Application {
    id: String,
    socket_path: PathBuf,
    database: Database,
    logger: slog::Logger,
}

struct Database {
    available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
}

impl Application {
    pub fn new(id: &str, socket_path: &Path) -> Self {
        Self {
            id: id.to_string(),
            socket_path: socket_path.to_path_buf(),
            database: Database {
                available_signatures_registrations: Arc::new(Mutex::new(Vec::new())),
            },
            logger: slog_scope::logger().new(slog::o!("name" => "app")),
        }
    }

    pub async fn run(&self) -> StdResult<()> {
        info!(self.logger, "Running application with id: {}", self.id);

        let mut join_set = JoinSet::new();
        self.listen_to_termination_signals(&mut join_set);

        let (server_shutdown_tx, server_shutdown_rx) = oneshot::channel();
        self.spawn_http_server_on_socket(server_shutdown_rx, &mut join_set);

        let shutdown_reason = match join_set.join_next().await {
            Some(Err(e)) => {
                crit!(self.logger, "A critical error occurred: {e:?}");
                None
            }
            Some(Ok(res)) => res?,
            None => None,
        };

        join_set.shutdown().await;
        let _ = server_shutdown_tx.send(());

        debug!(self.logger, "Stopping"; "shutdown_reason" => shutdown_reason);

        Ok(())
    }

    fn spawn_http_server_on_socket(
        &self,
        shutdown_rx: oneshot::Receiver<()>,
        join_set: &mut JoinSet<StdResult<Option<String>>>,
    ) {
        let routes = router::routes();
        let socket_path = self.socket_path.clone();
        join_set.spawn(async move {
            let listener = UnixListener::bind(socket_path)?;
            let incoming = UnixListenerStream::new(listener);

            let server =
                warp::serve(routes).serve_incoming_with_graceful_shutdown(incoming, async {
                    shutdown_rx.await.ok();
                });
            server.await;

            Ok(None)
        });
    }

    fn listen_to_termination_signals(&self, join_set: &mut JoinSet<StdResult<Option<String>>>) {
        join_set.spawn(async {
            tokio::signal::ctrl_c()
                .await
                .map_err(|e| anyhow!(e))
                .map(|_| Some("Received Ctrl+C".to_string()))
        });

        join_set.spawn(async move {
            let mut sigterm =
                signal(SignalKind::terminate()).expect("Failed to create SIGTERM signal");
            sigterm
                .recv()
                .await
                .ok_or(anyhow!("Failed to receive SIGTERM"))
                .map(|_| Some("Received SIGTERM".to_string()))
        });

        join_set.spawn(async move {
            let mut sigterm = signal(SignalKind::quit()).expect("Failed to create SIGQUIT signal");
            sigterm
                .recv()
                .await
                .ok_or(anyhow!("Failed to receive SIGQUIT"))
                .map(|_| Some("Received SIGQUIT".to_string()))
        });
    }
}
