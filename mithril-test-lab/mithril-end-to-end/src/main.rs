use clap::Parser;
use mithril_end_to_end::Spec;
use mithril_end_to_end::{Devnet, MithrilInfrastructure};
use slog::{Drain, Logger};
use slog_scope::error;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Tests args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// A directory where all logs, generated devnet artefacts, snapshots and store folder
    /// will be located.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/mithril-end-to-end`
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
    #[clap(long, default_value_t = 1)]
    number_of_pool_nodes: u8,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger());
    let server_port = 8080;
    let work_dir = match args.work_directory {
        Some(path) => {
            create_workdir_if_not_exist_clean_otherwise(&path);
            path.canonicalize().unwrap()
        }
        None => {
            let work_dir = std::env::temp_dir().join("mithril_end_to_end");
            create_workdir_if_not_exist_clean_otherwise(&work_dir);
            work_dir.canonicalize().unwrap()
        }
    };

    let devnet = Devnet::bootstrap(
        args.devnet_scripts_directory,
        work_dir.join("devnet"),
        args.number_of_bft_nodes,
        args.number_of_pool_nodes,
    )
    .await?;

    let infrastructure =
        MithrilInfrastructure::start(server_port, devnet.clone(), &work_dir, &args.bin_directory)
            .await?;

    let mut spec = Spec::new(infrastructure);

    match spec.run().await {
        Ok(_) => {
            devnet.stop().await?;
            Ok(())
        }
        Err(error) => {
            let has_written_logs = spec.dump_processes_logs().await;
            error!("Mithril End to End test failed: {}", error);
            devnet.stop().await?;
            has_written_logs?;
            Err(error)
        }
    }
}

fn build_logger() -> Logger {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

fn create_workdir_if_not_exist_clean_otherwise(work_dir: &Path) {
    if work_dir.exists() {
        fs::remove_dir_all(&work_dir).expect("Previous work dir removal failed");
    }
    fs::create_dir(&work_dir).expect("Work dir creation failure");
}
