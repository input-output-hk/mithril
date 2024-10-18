use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::anyhow;
use slog::{crit, debug, info};
use tokio::net::UnixListener;
use tokio::signal::unix::{signal, SignalKind};
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio::task::JoinSet;
use tokio_stream::wrappers::UnixListenerStream;

use mithril_common::messages::RegisterSignatureMessage;
use mithril_common::StdResult;

use crate::entities::{Message, RouterDependencies};
use crate::message_listener::MessageListener;
use crate::message_sender::MessageSender;
use crate::server::router;
use crate::DirectoryObserver;

type AppJoinSet = JoinSet<StdResult<Option<String>>>;

pub struct Application {
    id: String,
    socket_path: PathBuf,
    input_directory: PathBuf,
    peers_input_directories: Vec<PathBuf>,
    database: Database,
    main_logger: slog::Logger,
}

struct Database {
    available_signatures_registrations: Arc<Mutex<Vec<RegisterSignatureMessage>>>,
}

impl Application {
    pub fn new(
        id: &str,
        socket_path: &Path,
        input_directory: &Path,
        peers_input_directories: Vec<PathBuf>,
        main_logger: &slog::Logger,
    ) -> Self {
        Self {
            id: id.to_string(),
            socket_path: socket_path.to_path_buf(),
            input_directory: input_directory.to_path_buf(),
            peers_input_directories,
            database: Database {
                available_signatures_registrations: Arc::new(Mutex::new(vec![])),
            },
            main_logger: main_logger.clone(),
        }
    }

    pub async fn run(&self) -> StdResult<()> {
        info!(self.main_logger, "Running application with id: {}", self.id);

        let mut join_set = AppJoinSet::new();
        self.listen_to_termination_signals(&mut join_set);

        let (from_input_dir_msg_tx, from_input_dir_msg_rx) = mpsc::channel(10);
        let (from_socket_msg_tx, from_socket_msg_rx) = mpsc::channel(10);

        // The observer will stop when dropped
        let _directory_observer = DirectoryObserver::watch(
            &self.input_directory,
            from_input_dir_msg_tx,
            &self.main_logger,
        )?;
        self.listen_input_folder_for_messages(from_input_dir_msg_rx, &mut join_set);

        let (server_shutdown_tx, server_shutdown_rx) = oneshot::channel();
        self.spawn_http_server_on_socket(server_shutdown_rx, from_socket_msg_tx, &mut join_set);

        self.forward_message_from_http_server_to_peers(from_socket_msg_rx, &mut join_set);

        let shutdown_reason = match join_set.join_next().await {
            Some(Err(e)) => {
                crit!(self.main_logger, "A critical error occurred: {e:?}");
                None
            }
            Some(Ok(res)) => res?,
            None => None,
        };

        join_set.shutdown().await;
        let _ = server_shutdown_tx.send(());

        debug!(self.main_logger, "Stopping"; "shutdown_reason" => shutdown_reason);

        Ok(())
    }

    fn forward_message_from_http_server_to_peers(
        &self,
        msg_rx: mpsc::Receiver<Message>,
        join_set: &mut AppJoinSet,
    ) {
        let mut message_sender = MessageSender::new(
            msg_rx,
            self.peers_input_directories.clone(),
            &self.main_logger,
        );
        join_set.spawn(async move {
            message_sender.listen().await;
            Ok(None)
        });
    }

    fn listen_input_folder_for_messages(
        &self,
        msg_rx: mpsc::Receiver<Message>,
        join_set: &mut AppJoinSet,
    ) {
        let mut message_listener = MessageListener::new(
            msg_rx,
            self.database.available_signatures_registrations.clone(),
            &self.main_logger,
        );
        join_set.spawn(async move {
            message_listener.listen().await;
            Ok(None)
        });
    }

    fn spawn_http_server_on_socket(
        &self,
        shutdown_rx: oneshot::Receiver<()>,
        incoming_messages_sender: mpsc::Sender<Message>,
        join_set: &mut AppJoinSet,
    ) {
        let routes = router::routes(
            RouterDependencies {
                available_signatures_registrations: self
                    .database
                    .available_signatures_registrations
                    .clone(),
                incoming_messages_sender,
            },
            &self.main_logger,
        );
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

    fn listen_to_termination_signals(&self, join_set: &mut AppJoinSet) {
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
