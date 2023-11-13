use clap::Parser;
use mithril_common::StdResult;
use mithril_end_to_end::{Devnet, MithrilInfrastructure, RunOnly, Spec};
use slog::{Drain, Logger};
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

    /// Number of BFT nodes in the devnet
    #[clap(long, default_value_t = 1)]
    number_of_bft_nodes: u8,

    /// Number of Pool nodes in the devnet
    #[clap(long, default_value_t = 2)]
    number_of_pool_nodes: u8,

    /// Length of a Cardano slot in the devnet (in s)
    #[clap(long, default_value_t = 0.08)]
    cardano_slot_length: f64,

    /// Length of a Cardano epoch in the devnet (in s)
    #[clap(long, default_value_t = 45.0)]
    cardano_epoch_length: f64,

    /// Mithril era to run
    #[clap(long, default_value = "thales")]
    mithril_era: String,

    /// Enable run only mode
    #[clap(long)]
    run_only: bool,

    /// Enable P2P network mode
    #[clap(long)]
    use_p2p_network: bool,

    /// Skip cardano binaries download
    #[clap(long)]
    skip_cardano_bin_download: bool,
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger());
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

    let devnet = Devnet::bootstrap(
        args.devnet_scripts_directory,
        work_dir.join("devnet"),
        args.number_of_bft_nodes,
        args.number_of_pool_nodes,
        args.cardano_slot_length,
        args.cardano_epoch_length,
        args.skip_cardano_bin_download,
    )
    .await?;

    let mut infrastructure = MithrilInfrastructure::start(
        server_port,
        devnet.clone(),
        &work_dir,
        &args.bin_directory,
        &args.mithril_era,
        run_only_mode,
        use_p2p_network_mode,
    )
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
            let has_written_logs = infrastructure.tail_logs(20).await;
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

fn build_logger() -> Logger {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

fn create_workdir_if_not_exist_clean_otherwise(work_dir: &Path) {
    if work_dir.exists() {
        fs::remove_dir_all(work_dir).expect("Previous work dir removal failed");
    }
    fs::create_dir(work_dir).expect("Work dir creation failure");
}
