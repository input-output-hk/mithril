use anyhow::{anyhow, Context};
use clap::{CommandFactory, Parser, Subcommand};
use slog::{Drain, Level, Logger};
use slog_scope::{error, info, warn};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};
use tokio::{
    signal::unix::{signal, SignalKind},
    sync::Mutex,
    task::JoinSet,
};

use mithril_common::StdResult;
use mithril_doc::GenerateDocCommands;
use mithril_end_to_end::{
    Devnet, DevnetBootstrapArgs, MithrilInfrastructure, MithrilInfrastructureConfig, RunOnly, Spec,
};

/// Tests args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// Available commands
    #[command(subcommand)]
    command: Option<EndToEndCommands>,

    /// A directory where all logs, generated devnet artifacts, snapshots and store folder
    /// will be located.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/mithril-end-to-end`
    /// Exception for MacOS: default is `./mithril-end-to-end` as the length of the temporary directory's path
    /// is too long. It causes the maximum path size of the node.sock file to be exceeded.
    #[clap(long)]
    work_directory: Option<PathBuf>,

    /// Directory containing scripts to bootstrap a devnet
    #[clap(long, default_value = "./devnet")]
    devnet_scripts_directory: PathBuf,

    /// Directory to the mithril binaries
    ///
    /// It must contains the binaries of the aggregator, signer and client.
    ///
    /// Defaults to current folder
    #[clap(long, default_value = ".")]
    bin_directory: PathBuf,

    /// Number of Pool nodes in the devnet
    #[clap(long, default_value_t = 3, value_parser = has_at_least_two_pool_nodes)]
    number_of_pool_nodes: u8,

    /// Length of a Cardano slot in the devnet (in s)
    #[clap(long, default_value_t = 0.10)]
    cardano_slot_length: f64,

    /// Length of a Cardano epoch in the devnet (in s)
    #[clap(long, default_value_t = 30.0)]
    cardano_epoch_length: f64,

    /// Cardano node version
    #[clap(long, default_value = "9.2.1")]
    cardano_node_version: String,

    /// Epoch at which hard fork to the latest Cardano era will be made (starts with the latest era by default)
    #[clap(long, default_value_t = 0)]
    cardano_hard_fork_latest_era_at_epoch: u16,

    /// Mithril run interval for nodes (in ms)
    #[clap(long, default_value_t = 125)]
    mithril_run_interval: u32,

    /// Mithril era to run
    #[clap(long, default_value = "thales")]
    mithril_era: String,

    /// Mithril next era to run
    #[clap(long)]
    mithril_next_era: Option<String>,

    /// Mithril re-genesis on era switch (used only when 'mithril_next_era' is set)
    #[clap(long, default_value_t = false)]
    mithril_era_regenesis_on_switch: bool,

    /// Mithril era reader adapter
    #[clap(long, default_value = "cardano-chain")]
    mithril_era_reader_adapter: String,

    /// Signed entity types parameters (discriminants names in an ordered comma separated list).
    #[clap(
        long,
        value_delimiter = ',',
        default_value = "CardanoTransactions,CardanoStakeDistribution"
    )]
    signed_entity_types: Vec<String>,

    /// Enable run only mode
    #[clap(long)]
    run_only: bool,

    /// Enable P2P network mode
    #[clap(long)]
    use_p2p_network: bool,

    /// Enable P2P passive relays in P2P mode
    #[clap(long, default_value = "true")]
    use_p2p_passive_relays: bool,

    /// Skip cardano binaries download
    #[clap(long)]
    skip_cardano_bin_download: bool,

    /// Verbosity level
    #[clap(
        short,
        long,
        action = clap::ArgAction::Count,
        help = "Verbosity level, add more v to increase"
    )]
    verbose: u8,
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

fn has_at_least_two_pool_nodes(s: &str) -> Result<u8, String> {
    let number_of_pool_nodes: u8 = s.parse().map_err(|_| format!("`{}` isn't a number", s))?;
    if number_of_pool_nodes >= 2 {
        Ok(number_of_pool_nodes)
    } else {
        Err(format!(
            "At least two pool nodes are required (one for the aggregator, one for at least one \
            signer), number given: {s}",
        ))
    }
}

#[derive(Subcommand, Debug, Clone)]
enum EndToEndCommands {
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger(&args));

    if let Some(EndToEndCommands::GenerateDoc(cmd)) = &args.command {
        return cmd
            .execute(&mut Args::command())
            .map_err(|message| anyhow!(message));
    }

    let work_dir = match &args.work_directory {
        Some(path) => {
            create_workdir_if_not_exist_clean_otherwise(path);
            path.canonicalize()?
        }
        None => {
            #[cfg(target_os = "macos")]
            let work_dir = PathBuf::from("./mithril_end_to_end");
            #[cfg(not(target_os = "macos"))]
            let work_dir = std::env::temp_dir().join("mithril_end_to_end");
            create_workdir_if_not_exist_clean_otherwise(&work_dir);
            work_dir.canonicalize()?
        }
    };
    let artifacts_dir = {
        let path = work_dir.join("artifacts");
        fs::create_dir(&path).expect("Artifacts dir creation failure");
        path
    };

    let mut app = App::new();
    let mut app_stopper = AppStopper::new(&app);
    let mut join_set = JoinSet::new();
    with_gracefull_shutdown(&mut join_set);

    join_set.spawn(async move { app.run(args, work_dir, artifacts_dir).await });

    let res = match join_set.join_next().await {
        Some(Ok(tasks_result)) => tasks_result,
        Some(Err(join_set_error)) => Err(anyhow!(join_set_error)).with_context(|| "JoinSet error"),
        None => Ok(()),
    };

    app_stopper.stop().await;
    join_set.shutdown().await;
    res
}

struct App {
    devnet: Arc<Mutex<Option<Devnet>>>,
    infrastructure: Arc<Mutex<Option<MithrilInfrastructure>>>,
}

impl App {
    fn new() -> Self {
        Self {
            devnet: Arc::new(Mutex::new(None)),
            infrastructure: Arc::new(Mutex::new(None)),
        }
    }

    async fn tail_logs(&self) {
        if let Some(infrastructure) = self.infrastructure.lock().await.as_ref() {
            let _ = infrastructure.tail_logs(40).await.inspect_err(|e| {
                error!("Failed to tail logs: {}", e);
            });
        }
    }

    pub async fn run(
        &mut self,
        args: Args,
        work_dir: PathBuf,
        artifacts_dir: PathBuf,
    ) -> StdResult<()> {
        let server_port = 8080;
        let run_only_mode = args.run_only;
        let use_p2p_network_mode = args.use_p2p_network;
        let use_p2p_passive_relays = args.use_p2p_passive_relays;

        let devnet = Devnet::bootstrap(&DevnetBootstrapArgs {
            devnet_scripts_dir: args.devnet_scripts_directory,
            artifacts_target_dir: work_dir.join("devnet"),
            number_of_pool_nodes: args.number_of_pool_nodes,
            cardano_slot_length: args.cardano_slot_length,
            cardano_epoch_length: args.cardano_epoch_length,
            cardano_node_version: args.cardano_node_version.to_owned(),
            cardano_hard_fork_latest_era_at_epoch: args.cardano_hard_fork_latest_era_at_epoch,
            skip_cardano_bin_download: args.skip_cardano_bin_download,
        })
        .await?;
        *self.devnet.lock().await = Some(devnet.clone());

        let mut infrastructure = MithrilInfrastructure::start(&MithrilInfrastructureConfig {
            server_port,
            devnet: devnet.clone(),
            artifacts_dir,
            work_dir,
            bin_dir: args.bin_directory,
            cardano_node_version: args.cardano_node_version,
            mithril_run_interval: args.mithril_run_interval,
            mithril_era: args.mithril_era,
            mithril_era_reader_adapter: args.mithril_era_reader_adapter,
            signed_entity_types: args.signed_entity_types.clone(),
            run_only_mode,
            use_p2p_network_mode,
            use_p2p_passive_relays,
            use_era_specific_work_dir: args.mithril_next_era.is_some(),
        })
        .await?;

        let runner: StdResult<()> = match run_only_mode {
            true => {
                let mut run_only = RunOnly::new(&mut infrastructure);
                run_only.start().await
            }
            false => {
                let mut spec = Spec::new(
                    &mut infrastructure,
                    args.signed_entity_types,
                    args.mithril_next_era,
                    args.mithril_era_regenesis_on_switch,
                );
                spec.run().await
            }
        };
        *self.infrastructure.lock().await = Some(infrastructure);

        match runner.with_context(|| "Mithril End to End test failed") {
            Ok(()) if run_only_mode => loop {
                info!("Mithril end to end is running and will remain active until manually stopped...");
                tokio::time::sleep(Duration::from_secs(5)).await;
            },
            Ok(()) => Ok(()),
            Err(error) => {
                self.tail_logs().await;
                Err(error)
            }
        }
    }
}

struct AppStopper {
    devnet: Arc<Mutex<Option<Devnet>>>,
    infrastructure: Arc<Mutex<Option<MithrilInfrastructure>>>,
}

impl AppStopper {
    pub fn new(app: &App) -> Self {
        Self {
            devnet: app.devnet.clone(),
            infrastructure: app.infrastructure.clone(),
        }
    }

    pub async fn stop(&mut self) {
        if let Some(infrastructure) = self.infrastructure.lock().await.as_mut() {
            let _ = infrastructure.stop_nodes().await.inspect_err(|e| {
                error!("Failed to stop nodes: {}", e);
            });
        }
        if let Some(devnet) = self.devnet.lock().await.as_ref() {
            let _ = devnet.stop().await.inspect_err(|e| {
                error!("Failed to stop devnet: {}", e);
            });
        }
    }
}

fn build_logger(args: &Args) -> Logger {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog::LevelFilter::new(drain, args.log_level()).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

fn create_workdir_if_not_exist_clean_otherwise(work_dir: &Path) {
    if work_dir.exists() {
        fs::remove_dir_all(work_dir).expect("Previous work dir removal failed");
    }
    fs::create_dir(work_dir).expect("Work dir creation failure");
}

fn with_gracefull_shutdown(join_set: &mut JoinSet<StdResult<()>>) {
    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).expect("Failed to create SIGTERM signal");
        sigterm
            .recv()
            .await
            .ok_or(anyhow!("Failed to receive SIGTERM"))
            .inspect(|()| warn!("Received SIGTERM"))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::interrupt()).expect("Failed to create SIGINT signal");
        sigterm
            .recv()
            .await
            .ok_or(anyhow!("Failed to receive SIGINT"))
            .inspect(|()| warn!("Received SIGINT"))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::quit()).expect("Failed to create SIGQUIT signal");
        sigterm
            .recv()
            .await
            .ok_or(anyhow!("Failed to receive SIGQUIT"))
            .inspect(|()| warn!("Received SIGQUIT"))
    });
}
