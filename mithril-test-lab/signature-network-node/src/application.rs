use anyhow::anyhow;
use slog::{crit, debug, info};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::signal::unix::{signal, SignalKind};
use tokio::sync::Mutex;
use tokio::task::JoinSet;

use mithril_common::messages::RegisterSignatureMessage;
use mithril_common::StdResult;

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

        let shutdown_reason = match join_set.join_next().await {
            Some(Err(e)) => {
                crit!(self.logger, "A critical error occurred: {e:?}");
                None
            }
            Some(Ok(res)) => res?,
            None => None,
        };

        join_set.shutdown().await;

        debug!(self.logger, "Stopping"; "shutdown_reason" => shutdown_reason);

        Ok(())
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
