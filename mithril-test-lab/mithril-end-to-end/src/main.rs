use anyhow::anyhow;
use clap::{CommandFactory, Parser, Subcommand};
use mithril_common::StdResult;
use mithril_doc::GenerateDocCommands;
use mithril_end_to_end::{
    Devnet, DevnetBootstrapArgs, MithrilInfrastructure, MithrilInfrastructureConfig, RunOnly, Spec,
};
use slog::{Drain, Level, Logger};
use slog_scope::{error, info};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    thread::sleep,
    time::Duration,
};
use tokio_util::sync::CancellationToken;

/// Tests args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// Available commands
    #[command(subcommand)]
    command: Option<EndToEndCommands>,

    /// A directory where all logs, generated devnet artefacts, snapshots and store folder
    /// will be located.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/mithril-end-to-end`
    /// Exception for MacOS: default is `./mithril-end-to-end` as the length of the temporary directory's path
    /// is too long. It causes the maximum path size of the node.sock file to be exceeded.
    #[clap(long)]
    work_directory: Option<PathBuf>,

    /// Directory containing scripts to boostrap a devnet
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
    #[clap(long, default_value_t = 0.08)]
    cardano_slot_length: f64,

    /// Length of a Cardano epoch in the devnet (in s)
    #[clap(long, default_value_t = 35.0)]
    cardano_epoch_length: f64,

    /// Cardano node version
    #[clap(long, default_value = "9.1.0")]
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

    /// Mithril era reader adapter
    #[clap(long, default_value = "cardano-chain")]
    mithril_era_reader_adapter: String,

    /// Signed entity types parameters (discriminants names in an ordered comma separated list).
    #[clap(long, value_delimiter = ',', default_value = "CardanoTransactions")]
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

    let server_port = 8080;
    let work_dir = match args.work_directory {
        Some(path) => {
            create_workdir_if_not_exist_clean_otherwise(&path);
            path.canonicalize().unwrap()
        }
        None => {
            #[cfg(target_os = "macos")]
            let work_dir = PathBuf::from("./mithril_end_to_end");
            #[cfg(not(target_os = "macos"))]
            let work_dir = std::env::temp_dir().join("mithril_end_to_end");
            create_workdir_if_not_exist_clean_otherwise(&work_dir);
            work_dir.canonicalize().unwrap()
        }
    };
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

    let mut infrastructure = MithrilInfrastructure::start(&MithrilInfrastructureConfig {
        server_port,
        devnet: devnet.clone(),
        work_dir,
        bin_dir: args.bin_directory,
        cardano_node_version: args.cardano_node_version,
        mithril_run_interval: args.mithril_run_interval,
        mithril_era: args.mithril_era,
        mithril_era_reader_adapter: args.mithril_era_reader_adapter,
        signed_entity_types: args.signed_entity_types,
        run_only_mode,
        use_p2p_network_mode,
        use_p2p_passive_relays,
    })
    .await?;

    let runner: StdResult<()> = match run_only_mode {
        true => {
            let mut run_only = RunOnly::new(&mut infrastructure);
            run_only.start().await
        }
        false => {
            let mut spec = Spec::new(&mut infrastructure);
            spec.run().await
        }
    };

    match runner {
        Ok(_) if run_only_mode => run_until_cancelled(devnet).await,
        Ok(_) => {
            devnet.stop().await?;
            Ok(())
        }
        Err(error) => {
            let has_written_logs = infrastructure.tail_logs(40).await;
            error!("Mithril End to End test in failed: {}", error);
            devnet.stop().await?;
            has_written_logs?;
            Err(error)
        }
    }
}

async fn run_until_cancelled(devnet: Devnet) -> StdResult<()> {
    let cancellation_token = CancellationToken::new();
    let cloned_token = cancellation_token.clone();

    tokio::select! {
        _ = tokio::spawn(async move {
            while !cloned_token.is_cancelled() {
                info!("Mithril end to end is running and will remain active until manually stopped...");
                sleep(Duration::from_secs(5));
            }
        }) => {}
        _ = tokio::signal::ctrl_c() => {
            cancellation_token.cancel();
            devnet.stop().await?;
        }
    }

    Ok(())
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
