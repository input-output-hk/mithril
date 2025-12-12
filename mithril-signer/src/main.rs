use anyhow::{Context, anyhow};
use clap::{CommandFactory, Parser, Subcommand};
use config::{Map, Value};

use slog::{Drain, Level, Logger, crit, debug, info, o};
use std::sync::Arc;
use std::time::Duration;
use std::{collections::HashMap, path::PathBuf};
use tokio::{
    signal::unix::{SignalKind, signal},
    sync::watch,
    task::JoinSet,
};

use mithril_common::StdResult;
use mithril_doc::{Documenter, DocumenterDefault, GenerateDocCommands, StructDoc};
use mithril_metric::MetricsServer;
use mithril_signer::{
    Configuration, DatabaseCommand, DefaultConfiguration, SignerRunner, SignerState, StateMachine,
    dependency_injection::DependenciesBuilder,
};

/// CLI args
#[derive(Documenter, Parser)]
#[clap(name = "mithril-signer")]
#[clap(about = "An implementation of a Mithril Signer", long_about = None)]
#[command(version)]
pub struct Args {
    /// Available commands
    #[command(subcommand)]
    command: Option<SignerCommands>,

    /// Run Mode
    #[clap(short, long, env("RUN_MODE"), default_value = "dev")]
    run_mode: String,

    /// Verbosity level
    #[clap(
        short,
        long,
        action = clap::ArgAction::Count,
        help = "Verbosity level, add more v to increase"
    )]
    #[example = "Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace`"]
    verbose: u8,

    /// Configuration file location
    #[clap(
        short,
        long,
        default_value = "./config",
        help = "Directory where the configuration file is located"
    )]
    configuration_dir: PathBuf,

    /// Disable immutables digests cache.
    #[clap(long)]
    disable_digests_cache: bool,

    /// If set the existing immutables digests cache will be reset.
    ///
    /// Will be ignored if set in conjunction with `--disable-digests-cache`.
    #[clap(long)]
    reset_digests_cache: bool,

    /// Enable metrics HTTP server (Prometheus endpoint on /metrics).
    #[clap(long, env = "ENABLE_METRICS_SERVER", default_value_t = false)]
    enable_metrics_server: bool,

    /// Metrics HTTP server IP.
    #[clap(long, env = "METRICS_SERVER_IP", default_value = "0.0.0.0")]
    metrics_server_ip: String,

    /// Metrics HTTP server listening port.
    #[clap(long, env = "METRICS_SERVER_PORT", default_value_t = 9090)]
    metrics_server_port: u16,

    /// If set no error is returned in case of unparsable block and an error log is written instead.
    ///
    /// Will be ignored on (pre)production networks.
    #[clap(long)]
    allow_unparsable_block: bool,

    /// Preloading refresh interval in seconds
    #[clap(
        long,
        env = "PRELOADING_REFRESH_INTERVAL_IN_SECONDS",
        default_value_t = 43200
    )]
    preloading_refresh_interval_in_seconds: u64,

    /// Number of retry attempts when publishing the signature
    #[clap(long, env = "SIGNATURE_PUBLISHER_RETRY_ATTEMPTS", default_value_t = 3)]
    signature_publisher_retry_attempts: u64,

    /// Delay (in milliseconds) between two retry attempts when publishing the signature
    #[clap(
        long,
        env = "SIGNATURE_PUBLISHER_RETRY_DELAY_MS",
        default_value_t = 2_000
    )]
    signature_publisher_retry_delay_ms: u64,

    /// Delay (in milliseconds) between two separate publications done by the delayer signature publisher
    #[clap(
        long,
        env = "SIGNATURE_PUBLISHER_DELAYER_DELAY_MS",
        default_value_t = 10_000
    )]
    signature_publisher_delayer_delay_ms: u64,

    /// Whether to skip the delayer when publishing the signature
    ///
    /// If set to true, the signatures will be published only once:
    /// - if the 'future_dmq` feature is used to compile, the signatures will be published only with the DMQ protocol
    /// - if the `future_dmq` feature is not used, the signatures will be published with the regular HTTP protocol

    #[clap(
        long,
        env = "SIGNATURE_PUBLISHER_SKIP_DELAYER",
        default_value_t = false
    )]
    signature_publisher_skip_delayer: bool,
}

impl Args {
    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
    }
}

fn build_logger(min_level: Level) -> Logger {
    let drain = slog_bunyan::with_name("mithril-signer", std::io::stdout())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, min_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), o!())
}

#[derive(Subcommand, Debug, Clone)]
enum SignerCommands {
    Database(DatabaseCommand),
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

fn format_crate_name_to_config_key() -> String {
    env!("CARGO_PKG_NAME").replace("-", "")
}

#[tokio::main]
async fn main() -> StdResult<()> {
    // Load args
    let args = Args::parse();
    let root_logger = build_logger(args.log_level());

    debug!(root_logger, "Starting"; "node_version" => env!("CARGO_PKG_VERSION"));

    if let Some(cmd) = &args.command {
        match cmd {
            SignerCommands::Database(cmd) => return cmd.execute(root_logger).await,
            SignerCommands::GenerateDoc(cmd) => {
                let config_infos = [
                    Args::extract(),
                    Configuration::extract(),
                    DefaultConfiguration::extract(),
                ];

                let mut iter_config = config_infos.iter();
                let mut merged_struct_doc = StructDoc::default();
                for next_config in &mut iter_config {
                    merged_struct_doc = merged_struct_doc.merge_struct_doc(next_config);
                }

                let mut configs_map = HashMap::new();
                configs_map.insert(format_crate_name_to_config_key(), merged_struct_doc);

                return cmd
                    .execute_with_configurations(&mut Args::command(), configs_map)
                    .map_err(|message| anyhow!(message));
            }
        }
    };

    // Load config
    let config = config::Config::builder()
        .set_default("disable_digests_cache", args.disable_digests_cache)
        .with_context(|| "configuration error: could not set `disable_digests_cache`")?
        .set_default("reset_digests_cache", args.reset_digests_cache)
        .with_context(|| "configuration error: could not set `reset_digests_cache`")?
        .set_default("enable_metrics_server", args.enable_metrics_server)
        .with_context(|| "configuration error: could not set `enable_metrics_server`")?
        .set_default("allow_unparsable_block", args.allow_unparsable_block)
        .with_context(|| "configuration error: could not set `allow_unparsable_block`")?
        .set_default(
            "preloading_refresh_interval_in_seconds",
            args.preloading_refresh_interval_in_seconds,
        )
        .with_context(
            || "configuration error: could not set `preloading_refresh_interval_in_seconds`",
        )?
        .set_default(
            "signature_publisher_config.retry_attempts",
            args.signature_publisher_retry_attempts,
        )?
        .set_default(
            "signature_publisher_config.retry_delay_ms",
            args.signature_publisher_retry_delay_ms,
        )?
        .set_default(
            "signature_publisher_config.delayer_delay_ms",
            args.signature_publisher_delayer_delay_ms,
        )?
        .set_default(
            "signature_publisher_config.skip_delayer",
            args.signature_publisher_skip_delayer,
        )?
        .add_source(DefaultConfiguration::default())
        .add_source(
            config::File::with_name(&format!(
                "{}/{}.json",
                args.configuration_dir.display(),
                args.run_mode
            ))
            .required(false),
        )
        .add_source(config::Environment::default())
        .build()
        .with_context(|| "configuration build error")?
        .try_deserialize()
        .with_context(|| "configuration deserialize error")?;

    let services = DependenciesBuilder::new(&config, root_logger.clone())
        .build()
        .await
        .with_context(|| "services initialization error")?;

    let metrics_service = services.metrics_service.clone();
    let cardano_transaction_preloader = services.cardano_transactions_preloader.clone();

    debug!(root_logger, "Started"; "run_mode" => &args.run_mode, "config" => format!("{config:?}"));

    let state_machine = StateMachine::new(
        SignerState::Init,
        Box::new(SignerRunner::new(
            config.clone(),
            services,
            root_logger.clone(),
        )),
        Duration::from_millis(config.run_interval),
        metrics_service.clone(),
        root_logger.clone(),
    );

    let (stop_tx, stop_rx) = watch::channel(());

    let mut join_set = JoinSet::new();
    join_set.spawn(async move { state_machine.run().await.map_err(|e| anyhow!(e)).map(|_| None) });

    let preload_logger = root_logger.clone();
    join_set.spawn(async move {
        let refresh_interval = config.preloading_refresh_interval_in_seconds;
        let mut interval = tokio::time::interval(Duration::from_secs(refresh_interval));
        loop {
            interval.tick().await;
            if let Err(err) = cardano_transaction_preloader.preload().await {
                crit!(preload_logger, "🔥 Cardano transactions preloader failed"; "error" => ?err);
            }
            info!(
                preload_logger,
                "⟳ Next Preload Cardano Transactions will start in {refresh_interval} s",
            );
        }
    });

    if config.enable_metrics_server {
        let metrics_logger = root_logger.clone();
        let stop_rx_clone = stop_rx.clone();
        join_set.spawn(async move {
            MetricsServer::build(
                &config.metrics_server_ip,
                config.metrics_server_port,
                metrics_service,
                metrics_logger.clone(),
            )
            .serve(stop_rx_clone)
            .await
            .map_err(|e| anyhow!(e))
            .map(|_| None)
        });
    }

    join_set.spawn(async {
        tokio::signal::ctrl_c()
            .await
            .map_err(|e| anyhow!(e))
            .map(|_| Some("Received Ctrl+C".to_string()))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).expect("Failed to create SIGTERM signal");
        sigterm
            .recv()
            .await
            .with_context(|| "Failed to receive SIGTERM")
            .map(|_| Some("Received SIGTERM".to_string()))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::quit()).expect("Failed to create SIGQUIT signal");
        sigterm
            .recv()
            .await
            .with_context(|| "Failed to receive SIGQUIT")
            .map(|_| Some("Received SIGQUIT".to_string()))
    });

    let shutdown_reason = match join_set.join_next().await {
        Some(Err(e)) => {
            crit!(root_logger, "A critical error occurred"; "error" => ?e);
            None
        }
        Some(Ok(res)) => res?,
        None => None,
    };

    stop_tx.send(()).with_context(|| "Stop signal could not be sent")?;

    join_set.shutdown().await;

    debug!(root_logger, "Stopping"; "shutdown_reason" => shutdown_reason);

    Ok(())
}
