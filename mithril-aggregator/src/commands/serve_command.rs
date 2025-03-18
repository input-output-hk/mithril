use std::{
    net::IpAddr,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{anyhow, Context};
use chrono::TimeDelta;
use clap::Parser;

use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value};

use slog::{crit, debug, info, warn, Logger};
use tokio::{sync::oneshot, task::JoinSet};

use mithril_cli_helper::{
    register_config_value, register_config_value_bool, register_config_value_option,
};
use mithril_common::StdResult;
use mithril_metric::MetricsServer;

use crate::{dependency_injection::DependenciesBuilder, tools::VacuumTracker, Configuration};

const VACUUM_MINIMUM_INTERVAL: TimeDelta = TimeDelta::weeks(1);

/// Server runtime mode
#[derive(Parser, Debug, Clone)]
pub struct ServeCommand {
    /// Server listening IP
    #[clap(long)]
    server_ip: Option<String>,

    /// Server TCP port
    #[clap(long)]
    server_port: Option<u16>,

    /// Directory to store snapshot
    /// Defaults to work folder
    #[clap(long)]
    snapshot_directory: Option<PathBuf>,

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

    /// Master aggregator endpoint
    ///
    /// This is the endpoint of the aggregator that will be used to fetch the latest epoch settings
    /// and store the signer registrations when the aggregator is running in a slave mode.
    /// If this is not set, the aggregator will run in a master mode.
    #[clap(long)]
    master_aggregator_endpoint: Option<String>,
}

impl Source for ServeCommand {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let namespace = "clap arguments".to_string();

        register_config_value_option!(result, &namespace, self.server_ip);
        register_config_value_option!(result, &namespace, self.server_port);
        register_config_value_option!(
            result,
            &namespace,
            self.snapshot_directory,
            |v: PathBuf| format!("{}", v.to_string_lossy())
        );
        register_config_value_bool!(result, &namespace, self.disable_digests_cache);
        register_config_value_bool!(result, &namespace, self.reset_digests_cache);
        register_config_value_bool!(result, &namespace, self.allow_unparsable_block);
        register_config_value_bool!(result, &namespace, self.enable_metrics_server);
        register_config_value_option!(result, &namespace, self.metrics_server_ip);
        register_config_value_option!(result, &namespace, self.metrics_server_port);
        // TODO is it normal to pass a Some(v) and not only v when value is present ?
        register_config_value_option!(
            result,
            &namespace,
            self.master_aggregator_endpoint,
            |v: String| { Some(v) }
        );

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
        let event_store_thread = tokio::spawn(async move { event_store.run().await.unwrap() });

        // start the database vacuum operation, if needed
        self.perform_database_vacuum_if_needed(
            &config.data_stores_directory,
            &mut dependencies_builder,
            VACUUM_MINIMUM_INTERVAL,
            &root_logger,
        )
        .await?;

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

        let mut usage_reporter = dependencies_builder
            .create_usage_reporter()
            .await
            .with_context(|| "Dependencies Builder can not create usage reporter")?;
        join_set.spawn(async move {
            let interval_duration =
                Duration::from_secs(config.persist_usage_report_interval_in_seconds);
            usage_reporter.run_forever(interval_duration).await;
            Ok(())
        });

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

    /// This function checks if a database vacuum is needed and performs it if necessary.
    ///
    /// Errors from [VacuumTracker] operations are logged but not propagated as errors.
    async fn perform_database_vacuum_if_needed(
        &self,
        store_dir: &Path,
        dependencies_builder: &mut DependenciesBuilder,
        vacuum_min_interval: TimeDelta,
        logger: &Logger,
    ) -> StdResult<()> {
        let vacuum_tracker = VacuumTracker::new(store_dir, vacuum_min_interval, logger.clone());
        match vacuum_tracker.check_vacuum_needed() {
            Ok((true, _)) => {
                info!(logger, "Performing vacuum");

                let upkeep = dependencies_builder
                    .get_upkeep_service()
                    .await
                    .with_context(|| "Dependencies Builder can not create upkeep")?;

                upkeep
                    .vacuum()
                    .await
                    .with_context(|| "Upkeep service failed to vacuum database")?;

                match vacuum_tracker.update_last_vacuum_time() {
                    Ok(last_vacuum) => {
                        info!(logger, "Vacuum performed"; "last_vacuum" => last_vacuum.to_rfc3339());
                    }
                    Err(e) => {
                        warn!(logger, "Failed to update last vacuum time"; "error" => ?e);
                    }
                }
            }
            Ok((false, last_vacuum)) => {
                let time_display =
                    last_vacuum.map_or_else(|| "never".to_string(), |time| time.to_rfc3339());
                info!(logger, "No vacuum needed"; "last_vacuum" => time_display);
            }
            Err(e) => {
                warn!(logger, "Failed to check if vacuum is needed"; "error" => ?e);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use config::ValueKind;

    use super::*;
    use std::collections::HashMap;

    // TODO : just here to check there is no regression with the old configuration.
    // We may remove it and probably all tests in this file when macros are finished
    impl ServeCommand {
        fn collect_legacy(&self) -> Result<Map<String, Value>, config::ConfigError> {
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
            if let Some(master_aggregator_endpoint) = self.master_aggregator_endpoint.clone() {
                result.insert(
                    "master_aggregator_endpoint".to_string(),
                    Value::new(
                        Some(&namespace),
                        ValueKind::from(Some(master_aggregator_endpoint)),
                    ),
                );
            }

            Ok(result)
        }
    }

    #[test]
    fn test_serve_command_collect() {
        let serve_command = ServeCommand {
            server_ip: Some("value_server_ip".to_string()),
            server_port: Some(8000),
            snapshot_directory: Some(PathBuf::from("/mithril/aggregator/")),
            disable_digests_cache: true,
            reset_digests_cache: true,
            allow_unparsable_block: true,
            enable_metrics_server: true,
            metrics_server_ip: Some("value_metrics_server_ip".to_string()),
            metrics_server_port: Some(8080),
            master_aggregator_endpoint: Some("value_master_aggregator_endpoint".to_string()),
        };

        let result = serve_command.collect().unwrap().clone();
        let mut expected = HashMap::new();
        expected.insert(
            "server_ip".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from("value_server_ip"),
            ),
        );
        expected.insert(
            "server_port".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from(8000_u64),
            ),
        );
        expected.insert(
            "snapshot_directory".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from("/mithril/aggregator/"),
            ),
        );
        expected.insert(
            "disable_digests_cache".to_string(),
            Value::new(Some(&"clap arguments".to_string()), ValueKind::from(true)),
        );
        expected.insert(
            "reset_digests_cache".to_string(),
            Value::new(Some(&"clap arguments".to_string()), ValueKind::from(true)),
        );
        expected.insert(
            "allow_unparsable_block".to_string(),
            Value::new(Some(&"clap arguments".to_string()), ValueKind::from(true)),
        );
        expected.insert(
            "enable_metrics_server".to_string(),
            Value::new(Some(&"clap arguments".to_string()), ValueKind::from(true)),
        );
        expected.insert(
            "metrics_server_ip".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from("value_metrics_server_ip"),
            ),
        );
        expected.insert(
            "metrics_server_port".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from(Some(8080_u64)),
            ),
        );
        expected.insert(
            "master_aggregator_endpoint".to_string(),
            Value::new(
                Some(&"clap arguments".to_string()),
                ValueKind::from("value_master_aggregator_endpoint"),
            ),
        );

        assert_eq!(expected, result);

        assert_eq!(serve_command.collect_legacy().unwrap(), result);
    }

    #[test]
    fn test_serve_command_collect_when_empty_values() {
        let serve_command = ServeCommand {
            server_ip: None,
            server_port: None,
            snapshot_directory: None,
            disable_digests_cache: false,
            reset_digests_cache: false,
            allow_unparsable_block: false,
            enable_metrics_server: false,
            metrics_server_ip: None,
            metrics_server_port: None,
            master_aggregator_endpoint: None,
        };

        let result = serve_command.collect().unwrap().clone();
        let expected = HashMap::new();

        assert_eq!(expected, result);

        assert_eq!(serve_command.collect_legacy().unwrap(), result);
    }
}
