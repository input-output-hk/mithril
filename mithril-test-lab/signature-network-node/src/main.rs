mod application;
mod directory_observer;
mod entities;

use clap::Parser;
use slog::{o, Drain, Level, Logger};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::StdResult;

use crate::application::Application;

#[derive(Parser, Debug, Clone)]
pub struct Args {
    #[clap(long)]
    id: String,

    /// Path to the socket file to communicate with this node.
    ///
    /// If given the host directory must exist and existing socket file will be removed.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/signatures-network-node-{id}/node.sock`
    #[clap(long)]
    socket_path: Option<PathBuf>,

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

    fn socket_path(&self) -> PathBuf {
        match &self.socket_path {
            Some(path) => {
                if path.exists() {
                    fs::remove_file(&path).expect("Previous socket file removal failed");
                }
                path.to_path_buf()
            }
            None => {
                let parent_dir =
                    std::env::temp_dir().join(format!("signatures-network-node-{}", self.id));
                if parent_dir.exists() {
                    fs::remove_dir_all(&parent_dir).expect("Previous socket dir removal failed");
                }
                fs::create_dir(&parent_dir).expect("Socket dir creation failure");
                parent_dir.join("node.sock")
            }
        }
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(build_logger(args.log_level()));

    Application::new(&args.id, &args.socket_path()).run().await
}

fn build_logger(min_level: Level) -> Logger {
    let drain = slog_bunyan::new(std::io::stdout())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, min_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), o!())
}
