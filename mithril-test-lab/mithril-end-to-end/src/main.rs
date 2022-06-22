use clap::Parser;
use mithril_end_to_end::Spec;
use mithril_end_to_end::{Devnet, MithrilInfrastructure};
use slog::{Drain, Logger};
use slog_scope::error;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

/// Tests args
#[derive(Parser, Debug, Clone)]
pub struct Args {
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
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger());
    let work_dir = get_work_dir();
    let server_port = 8080;

    let devnet =
        Devnet::bootstrap(args.devnet_scripts_directory, work_dir.join("devnet"), 1, 2).await?;

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
            spec.dump_processes_logs().await?;
            error!("Mithril End to End test failed: {}", error);
            devnet.stop().await?;
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

fn get_work_dir() -> PathBuf {
    let work_dir = std::env::temp_dir().join("mithril_end_to_end");

    if work_dir.exists() {
        fs::remove_dir_all(&work_dir).expect("Previous work dir removal failed");
    }
    fs::create_dir(&work_dir).expect("Work dir creation failure");

    work_dir
}
