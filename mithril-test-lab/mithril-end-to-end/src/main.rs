use anyhow::{anyhow, Context};
use clap::{CommandFactory, Parser, Subcommand};
use slog::{Drain, Level, Logger};
use slog_scope::{error, info};
use std::{
    fmt, fs,
    path::{Path, PathBuf},
    process::{ExitCode, Termination},
    sync::Arc,
    time::Duration,
};
use thiserror::Error;
use tokio::{
    signal::unix::{signal, SignalKind},
    sync::Mutex,
    task::JoinSet,
};

use mithril_common::StdResult;
use mithril_doc::GenerateDocCommands;
use mithril_end_to_end::{
    Devnet, DevnetBootstrapArgs, MithrilInfrastructure, MithrilInfrastructureConfig,
    RetryableDevnetError, RunOnly, Spec,
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

    /// Number of aggregators
    #[clap(long, default_value_t = 1, value_parser = clap::value_parser!(u8).range(1..))]
    number_of_aggregators: u8,

    /// Number of signers
    #[clap(long, default_value_t = 2, value_parser = clap::value_parser!(u8).range(1..))]
    number_of_signers: u8,

    /// Length of a Cardano slot in the devnet (in s)
    #[clap(long, default_value_t = 0.10)]
    cardano_slot_length: f64,

    /// Length of a Cardano epoch in the devnet (in s)
    #[clap(long, default_value_t = 30.0)]
    cardano_epoch_length: f64,

    /// Cardano node version
    #[clap(long, default_value = "10.2.1")]
    cardano_node_version: String,

    /// Epoch at which hard fork to the latest Cardano era will be made (starts with the latest era by default)
    #[clap(long, default_value_t = 0)]
    cardano_hard_fork_latest_era_at_epoch: u16,

    /// Mithril run interval for nodes (in ms)
    #[clap(long, default_value_t = 125)]
    mithril_run_interval: u32,

    /// Mithril era to run
    #[clap(long, default_value = "pythagoras")]
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
        default_value = "CardanoTransactions,CardanoStakeDistribution,CardanoDatabase"
    )]
    signed_entity_types: Vec<String>,

    /// Enable run only mode
    #[clap(long)]
    run_only: bool,

    /// Use Mithril relays
    #[clap(long)]
    use_relays: bool,

    /// Signer registration relay mode (used only when 'use_relays' is set, can be 'passthrough' or 'p2p')
    #[clap(long, default_value = "passthrough")]
    relay_signer_registration_mode: String,

    /// Signature registration relay mode (used only when 'use_relays' is set, can be 'passthrough' or 'p2p')
    #[clap(long, default_value = "p2p")]
    relay_signature_registration_mode: String,

    /// Enable P2P passive relays in P2P mode (used only when 'use_relays' is set)
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

    fn validate(&self) -> StdResult<()> {
        if !self.use_relays && self.number_of_aggregators >= 2 {
            return Err(anyhow!(
                "The 'use_relays' parameter must be activated to run more than one aggregator"
            ));
        }

        Ok(())
    }
}

#[derive(Subcommand, Debug, Clone)]
enum EndToEndCommands {
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

fn main() -> AppResult {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async { main_exec().await })
        .into()
}

async fn main_exec() -> StdResult<()> {
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
        fs::create_dir(&path).with_context(|| "Artifacts dir creation failure")?;
        path
    };
    let store_dir = {
        let path = work_dir.join("stores");
        fs::create_dir(&path).with_context(|| "Stores dir creation failure")?;
        path
    };

    let mut app = App::new();
    let mut app_stopper = AppStopper::new(&app);
    let mut join_set = JoinSet::new();
    with_graceful_shutdown(&mut join_set);

    join_set.spawn(async move { app.run(args, work_dir, store_dir, artifacts_dir).await });

    let res = match join_set.join_next().await {
        Some(Ok(tasks_result)) => tasks_result,
        Some(Err(join_set_error)) => Err(anyhow!(join_set_error)).with_context(|| "JoinSet error"),
        None => Ok(()),
    };

    app_stopper.stop().await;
    join_set.shutdown().await;

    res
}

#[derive(Debug)]
enum AppResult {
    Success(),
    UnretryableError(anyhow::Error),
    RetryableError(anyhow::Error),
    Cancelled(anyhow::Error),
}

impl AppResult {
    fn exit_code(&self) -> ExitCode {
        match self {
            AppResult::Success() => ExitCode::SUCCESS,
            AppResult::UnretryableError(_) | AppResult::Cancelled(_) => ExitCode::FAILURE,
            AppResult::RetryableError(_) => ExitCode::from(2),
        }
    }
}

impl fmt::Display for AppResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AppResult::Success() => write!(f, "Success"),
            AppResult::UnretryableError(error) => write!(f, "Error(Unretryable): {error:?}"),
            AppResult::RetryableError(error) => write!(f, "Error(Retryable): {error:?}"),
            AppResult::Cancelled(error) => write!(f, "Cancelled: {error:?}"),
        }
    }
}

impl Termination for AppResult {
    fn report(self) -> ExitCode {
        let exit_code = self.exit_code();
        println!(" ");
        println!("{:-^100}", "");
        println!("Mithril End to End test outcome:");
        println!("{:-^100}", "");
        println!("{self}");

        exit_code
    }
}

impl From<StdResult<()>> for AppResult {
    fn from(result: StdResult<()>) -> Self {
        match result {
            Ok(()) => AppResult::Success(),
            Err(error) => {
                if error.is::<RetryableDevnetError>() {
                    AppResult::RetryableError(error)
                } else if error.is::<SignalError>() {
                    AppResult::Cancelled(error)
                } else {
                    AppResult::UnretryableError(error)
                }
            }
        }
    }
}

struct App {
    devnet: Arc<Mutex<Option<Devnet>>>,
    infrastructure: Arc<Mutex<Option<Arc<MithrilInfrastructure>>>>,
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

    async fn last_error_in_logs(&self) {
        if let Some(infrastructure) = self.infrastructure.lock().await.as_ref() {
            let _ = infrastructure.last_error_in_logs(1).await.inspect_err(|e| {
                error!("Failed to grep error in logs: {}", e);
            });
        }
    }

    pub async fn run(
        &mut self,
        args: Args,
        work_dir: PathBuf,
        store_dir: PathBuf,
        artifacts_dir: PathBuf,
    ) -> StdResult<()> {
        let server_port = 8080;
        args.validate()?;
        let run_only_mode = args.run_only;
        let use_relays = args.use_relays;
        let relay_signer_registration_mode = args.relay_signer_registration_mode;
        let relay_signature_registration_mode = args.relay_signature_registration_mode;

        let use_p2p_passive_relays = args.use_p2p_passive_relays;

        let devnet = Devnet::bootstrap(&DevnetBootstrapArgs {
            devnet_scripts_dir: args.devnet_scripts_directory,
            artifacts_target_dir: work_dir.join("devnet"),
            number_of_pool_nodes: args.number_of_aggregators + args.number_of_signers,
            cardano_slot_length: args.cardano_slot_length,
            cardano_epoch_length: args.cardano_epoch_length,
            cardano_node_version: args.cardano_node_version.to_owned(),
            cardano_hard_fork_latest_era_at_epoch: args.cardano_hard_fork_latest_era_at_epoch,
            skip_cardano_bin_download: args.skip_cardano_bin_download,
        })
        .await?;
        *self.devnet.lock().await = Some(devnet.clone());

        let infrastructure = Arc::new(
            MithrilInfrastructure::start(&MithrilInfrastructureConfig {
                number_of_aggregators: args.number_of_aggregators,
                number_of_signers: args.number_of_signers,
                server_port,
                devnet: devnet.clone(),
                work_dir,
                store_dir,
                artifacts_dir,
                bin_dir: args.bin_directory,
                cardano_node_version: args.cardano_node_version,
                mithril_run_interval: args.mithril_run_interval,
                mithril_era: args.mithril_era,
                mithril_era_reader_adapter: args.mithril_era_reader_adapter,
                signed_entity_types: args.signed_entity_types.clone(),
                run_only_mode,
                use_relays,
                relay_signer_registration_mode,
                relay_signature_registration_mode,
                use_p2p_passive_relays,
                use_era_specific_work_dir: args.mithril_next_era.is_some(),
            })
            .await?,
        );
        *self.infrastructure.lock().await = Some(infrastructure.clone());

        let runner: StdResult<()> = match run_only_mode {
            true => RunOnly::new(infrastructure).run().await,
            false => {
                Spec::new(
                    infrastructure,
                    args.signed_entity_types,
                    args.mithril_next_era,
                    args.mithril_era_regenesis_on_switch,
                )
                .run()
                .await
            }
        };

        match runner.with_context(|| "Mithril End to End test failed") {
            Ok(()) if run_only_mode => loop {
                info!("Mithril end to end is running and will remain active until manually stopped...");
                tokio::time::sleep(Duration::from_secs(5)).await;
            },
            Ok(()) => Ok(()),
            Err(error) => {
                self.tail_logs().await;
                self.last_error_in_logs().await;
                Err(error)
            }
        }
    }
}

struct AppStopper {
    devnet: Arc<Mutex<Option<Devnet>>>,
    infrastructure: Arc<Mutex<Option<Arc<MithrilInfrastructure>>>>,
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

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Signal received: `{0}`")]
pub struct SignalError(pub String);

fn with_graceful_shutdown(join_set: &mut JoinSet<StdResult<()>>) {
    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::terminate()).expect("Failed to create SIGTERM signal");
        sigterm.recv().await;

        Err(anyhow!(SignalError("SIGTERM".to_string())))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::interrupt()).expect("Failed to create SIGINT signal");
        sigterm.recv().await;

        Err(anyhow!(SignalError("SIGINT".to_string())))
    });

    join_set.spawn(async move {
        let mut sigterm = signal(SignalKind::quit()).expect("Failed to create SIGQUIT signal");
        sigterm.recv().await;

        Err(anyhow!(SignalError("SIGQUIT".to_string())))
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn app_result_exit_code() {
        let expected_exit_code = ExitCode::SUCCESS;
        let exit_code = AppResult::Success().exit_code();
        assert_eq!(expected_exit_code, exit_code);

        let expected_exit_code = ExitCode::FAILURE;
        let exit_code = AppResult::UnretryableError(anyhow::anyhow!("an error")).exit_code();
        assert_eq!(expected_exit_code, exit_code);

        let expected_exit_code = ExitCode::from(2);
        let exit_code = AppResult::RetryableError(anyhow::anyhow!("an error")).exit_code();
        assert_eq!(expected_exit_code, exit_code);

        let expected_exit_code = ExitCode::FAILURE;
        let exit_code = AppResult::Cancelled(anyhow::anyhow!("an error")).exit_code();
        assert_eq!(expected_exit_code, exit_code);
    }

    #[test]
    fn app_result_conversion() {
        assert!(matches!(AppResult::from(Ok(())), AppResult::Success()));

        assert!(matches!(
            AppResult::from(Err(anyhow!(RetryableDevnetError("an error".to_string())))),
            AppResult::RetryableError(_)
        ));

        assert!(matches!(
            AppResult::from(Err(anyhow!("an error"))),
            AppResult::UnretryableError(_)
        ));

        assert!(matches!(
            AppResult::from(Err(anyhow!(SignalError("an error".to_string())))),
            AppResult::Cancelled(_)
        ));
    }

    #[test]
    fn args_fails_validation() {
        let args = Args::parse_from(["", "--number-of-aggregators", "2"]);
        args.validate().expect_err(
            "validate should fail with more than one aggregator if p2p network is not used",
        );

        let args = Args::parse_from(["", "--use-relays", "--number-of-aggregators", "2"]);
        args.validate()
            .expect("validate should succeed with more than one aggregator if p2p network is used");
    }
}
