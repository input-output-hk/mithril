use anyhow::{anyhow, Context};
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use mithril_common::StdResult;
use mithril_metric::MetricsServer;
use slog::{crit, debug, info, warn, Logger};
use std::time::Duration;
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

    /// If set no error is returned in case of unparsable block and an error log is written instead.
    ///
    /// Will be ignored on (pre)production networks.
    #[clap(long)]
    allow_unparsable_block: bool,

    /// Enable metrics HTTP server (Prometheus endpoint on /metrics).
    #[clap(long)]
    enable_metrics_server: bool,

    /// Metrics HTTP server IP.
    #[clap(long)]
    metrics_server_ip: Option<String>,

    /// Metrics HTTP server listening port.
    #[clap(long)]
    metrics_server_port: Option<u16>,
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
        if self.disable_digests_cache {
            result.insert(
                "disable_digests_cache".to_string(),
                Value::new(Some(&namespace), ValueKind::from(true)),
            );
        };
        if self.reset_digests_cache {
            result.insert(
                "reset_digests_cache".to_string(),
                Value::new(Some(&namespace), ValueKind::from(true)),
            );
        }
        if self.allow_unparsable_block {
            result.insert(
                "allow_unparsable_block".to_string(),
                Value::new(Some(&namespace), ValueKind::from(true)),
            );
        };
        if self.enable_metrics_server {
            result.insert(
                "enable_metrics_server".to_string(),
                Value::new(Some(&namespace), ValueKind::from(true)),
            );
        };
        if let Some(metrics_server_ip) = self.metrics_server_ip.clone() {
            result.insert(
                "metrics_server_ip".to_string(),
                Value::new(Some(&namespace), ValueKind::from(metrics_server_ip)),
            );
        }
        if let Some(metrics_server_port) = self.metrics_server_port {
            result.insert(
                "metrics_server_port".to_string(),
                Value::new(Some(&namespace), ValueKind::from(metrics_server_port)),
            );
        }

        Ok(result)
    }
}

impl ServeCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        mut config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        config_builder = config_builder.add_source(self.clone());
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!(root_logger, "SERVE command"; "config" => format!("{config:?}"));
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), config.clone());

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

        // start the cardano transactions preloader
        let cardano_transactions_preloader = dependencies_builder
            .create_cardano_transactions_preloader()
            .await
            .with_context(|| {
                "Dependencies Builder can not create cardano transactions preloader"
            })?;
        let preload_task =
            tokio::spawn(async move { cardano_transactions_preloader.preload().await });

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

        // Create a SignersImporter only if the `cexplorer_pools_url` is provided in the config.
        if let Some(cexplorer_pools_url) = config.cexplorer_pools_url {
            match dependencies_builder
                .create_signer_importer(&cexplorer_pools_url)
                .await
            {
                Ok(service) => {
                    join_set.spawn(async move {
                        // Wait 5s to let the other services the time to start before running
                        // the first import.
                        tokio::time::sleep(Duration::from_secs(5)).await;
                        service
                            .run_forever(Duration::from_secs(
                                // Import interval are in minutes
                                config.signer_importer_run_interval * 60,
                            ))
                            .await;
                        Ok(())
                    });
                }
                Err(error) => {
                    warn!(
                        root_logger, "Failed to build the `SignersImporter`";
                        "url_to_import" => cexplorer_pools_url,
                        "error" => ?error
                    );
                }
            }
        }
        let metrics_service = dependencies_builder
            .get_metrics_service()
            .await
            .with_context(|| "Metrics service initialization error")?;
        let (metrics_server_shutdown_tx, metrics_server_shutdown_rx) = oneshot::channel();
        if config.enable_metrics_server {
            let metrics_logger = root_logger.clone();
            join_set.spawn(async move {
                let _ = MetricsServer::new(
                    &config.metrics_server_ip,
                    config.metrics_server_port,
                    metrics_service,
                    metrics_logger.clone(),
                )
                .start(metrics_server_shutdown_rx)
                .await
                .map_err(|e| anyhow!(e));

                Ok(())
            });
        }

        if let Some(signature_consumer) = dependencies_builder
            .create_signature_consumer()
            .await
            .with_context(|| "Dependencies Builder can not create signature consumer")?
        {
            join_set.spawn(async move {
                signature_consumer.listen().await;
                Ok(())
            });
        }

        join_set.spawn(async { tokio::signal::ctrl_c().await.map_err(|e| e.to_string()) });
        dependencies_builder.vanish().await;

        if let Err(e) = join_set.join_next().await.unwrap()? {
            crit!(root_logger, "A critical error occurred"; "error" => e);
        }

        metrics_server_shutdown_tx
            .send(())
            .map_err(|e| anyhow!("Metrics server shutdown signal could not be sent: {e:?}"))?;

        // stop servers
        join_set.shutdown().await;
        let _ = shutdown_tx.send(());

        if !preload_task.is_finished() {
            preload_task.abort();
        }

        info!(root_logger, "Event store is finishing...");
        event_store_thread.await.unwrap();
        println!("Services stopped, exiting.");

        Ok(())
    }
}
