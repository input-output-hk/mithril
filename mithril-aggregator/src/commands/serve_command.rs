use anyhow::Context;
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use mithril_common::StdResult;
use slog_scope::{crit, debug, info};
use std::{net::IpAddr, path::PathBuf};
use tokio::{sync::oneshot, task::JoinSet};

use crate::{dependency_injection::DependenciesBuilder, Configuration};

const SQLITE_MONITORING_FILE: &str = "monitoring.sqlite3";

/// Server runtime mode
#[derive(Parser, Debug, Clone)]
pub struct ServeCommand {
    /// Server listening IP
    #[clap(long)]
    pub server_ip: Option<String>,

    /// Server TCP port
    #[clap(long)]
    pub server_port: Option<u16>,

    /// Directory to store snapshot
    /// Defaults to work folder
    #[clap(long)]
    pub snapshot_directory: Option<PathBuf>,

    /// Disable immutables digests cache.
    #[clap(long)]
    disable_digests_cache: bool,

    /// If set the existing immutables digests cache will be reset.
    ///
    /// Will be ignored if set in conjunction with `--disable-digests-cache`.
    #[clap(long)]
    reset_digests_cache: bool,
}

impl Source for ServeCommand {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(server_ip) = self.server_ip.clone() {
            result.insert(
                "server_ip".to_string(),
                Value::new(Some(&namespace), ValueKind::from(server_ip)),
            );
        }
        if let Some(server_port) = self.server_port {
            result.insert(
                "server_port".to_string(),
                Value::new(Some(&namespace), ValueKind::from(server_port)),
            );
        }
        if let Some(snapshot_directory) = self.snapshot_directory.clone() {
            result.insert(
                "snapshot_directory".to_string(),
                Value::new(
                    Some(&namespace),
                    ValueKind::from(format!("{}", snapshot_directory.to_string_lossy())),
                ),
            );
        }

        Ok(result)
    }
}

impl ServeCommand {
    pub async fn execute(&self, mut config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        config_builder = config_builder.add_source(self.clone());
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!("SERVE command"; "config" => format!("{config:?}"));
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());

        // start servers
        println!("Starting server...");
        println!("Press Ctrl+C to stop");

        // start the monitoring thread
        let mut event_store = dependencies_builder
            .create_event_store()
            .await
            .with_context(|| "Dependencies Builder can not create event store")?;
        let event_store_config = config.clone();
        let event_store_thread = tokio::spawn(async move {
            event_store
                .run(Some(
                    event_store_config
                        .get_sqlite_dir()
                        .join(SQLITE_MONITORING_FILE),
                ))
                .await
                .unwrap()
        });

        // start the aggregator runtime
        let mut runtime = dependencies_builder
            .create_aggregator_runner()
            .await
            .with_context(|| "Dependencies Builder can not create aggregator runner")?;
        let mut join_set = JoinSet::new();
        join_set.spawn(async move { runtime.run().await.map_err(|e| e.to_string()) });

        // start the HTTP server
        let (shutdown_tx, shutdown_rx) = oneshot::channel();
        let routes = dependencies_builder
            .create_http_routes()
            .await
            .with_context(|| "Dependencies Builder can not create http routes")?;
        join_set.spawn(async move {
            let (_, server) = warp::serve(routes).bind_with_graceful_shutdown(
                (
                    config.server_ip.clone().parse::<IpAddr>().unwrap(),
                    config.server_port,
                ),
                async {
                    shutdown_rx.await.ok();
                },
            );
            server.await;

            Ok(())
        });
        join_set.spawn(async { tokio::signal::ctrl_c().await.map_err(|e| e.to_string()) });
        dependencies_builder.vanish();

        if let Err(e) = join_set.join_next().await.unwrap()? {
            crit!("A critical error occurred: {e}");
        }

        // stop servers
        join_set.shutdown().await;
        let _ = shutdown_tx.send(());

        info!("Event store is finishing...");
        event_store_thread.await.unwrap();
        println!("Services stopped, exiting.");

        Ok(())
    }
}
