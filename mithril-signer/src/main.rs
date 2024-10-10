use anyhow::{anyhow, Context};
use clap::{CommandFactory, Parser, Subcommand};
use config::{Map, Value};

use slog::{crit, debug, info, o, Drain, Level, Logger};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::{
    signal::unix::{signal, SignalKind},
    sync::oneshot,
    task::JoinSet,
};

use mithril_common::StdResult;
use mithril_doc::{Documenter, DocumenterDefault, GenerateDocCommands, StructDoc};
use mithril_signer::dependency_injection::DependenciesBuilder;
use mithril_signer::{
    Configuration, DefaultConfiguration, MetricsServer, SignerRunner, SignerState, StateMachine,
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
    // TODO: Replace the default value to 43200 (12 hours) once the Cardano transactions is activated on mainnet
    #[clap(
        long,
        env = "PRELOADING_REFRESH_INTERVAL_IN_SECONDS",
        default_value_t = 7200
    )]
    preloading_refresh_interval_in_seconds: u64,
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
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

#[tokio::main]
async fn main() -> StdResult<()> {
    // Load args
    let args = Args::parse();
    let root_logger = build_logger(args.log_level());

    if let Some(SignerCommands::GenerateDoc(cmd)) = &args.command {
        let config_infos = vec![
            Args::extract(),
            Configuration::extract(),
            DefaultConfiguration::extract(),
        ];
        return cmd
            .execute_with_configurations(&mut Args::command(), &config_infos)
            .map_err(|message| anyhow!(message));
    }

    #[cfg(feature = "bundle_openssl")]
    openssl_probe::init_ssl_cert_env_vars();

    debug!(root_logger, "Starting"; "node_version" => env!("CARGO_PKG_VERSION"));

    // Load config
    let config: Configuration = config::Config::builder()
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
        .with_context(|| {
            "configuration error: could not set `preloading_refresh_interval_in_seconds`"
        })?
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

    let mut join_set = JoinSet::new();
    join_set.spawn(async move {
        state_machine
            .run()
            .await
            .map_err(|e| anyhow!(e))
            .map(|_| None)
    });

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

    let (metrics_server_shutdown_tx, metrics_server_shutdown_rx) = oneshot::channel();
    if config.enable_metrics_server {
        let metrics_logger = root_logger.clone();
        join_set.spawn(async move {
            MetricsServer::new(
                &config.metrics_server_ip,
                config.metrics_server_port,
                metrics_service,
                metrics_logger.clone(),
            )
            .start(metrics_server_shutdown_rx)
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

    let shutdown_reason = match join_set.join_next().await {
        Some(Err(e)) => {
            crit!(root_logger, "A critical error occurred"; "error" => ?e);
            None
        }
        Some(Ok(res)) => res?,
        None => None,
    };

    metrics_server_shutdown_tx
        .send(())
        .map_err(|e| anyhow!("Metrics server shutdown signal could not be sent: {e:?}"))?;

    join_set.shutdown().await;

    debug!(root_logger, "Stopping"; "shutdown_reason" => shutdown_reason);

    Ok(())
}
