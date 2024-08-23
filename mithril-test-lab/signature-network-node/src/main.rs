use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use clap::Parser;
use slog::{o, Drain, Level, Logger};

use mithril_common::StdResult;

use signature_network_node::Application;

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

    /// Path to the directory where the node will listen for new messages.
    ///
    /// If given the directory will be created if it does not exist, if it exists files
    /// won't be removed.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/signatures-network-node-{id}/input/`
    #[clap(long)]
    input_directory: Option<PathBuf>,

    /// Verbosity level
    #[clap(
        short,
        long,
        action = clap::ArgAction::Count,
        help = "Verbosity level, add more v to increase"
    )]
    verbose: u8,
}

struct SanitizedArgs {
    id: String,
    socket_path: PathBuf,
    input_directory: PathBuf,
    verbose: u8,
}

impl Args {
    fn sanitize(mut self) -> StdResult<SanitizedArgs> {
        self.sanitize_paths()?;

        Ok(SanitizedArgs {
            id: self.id.clone(),
            socket_path: self.socket_path.unwrap(),
            input_directory: self.input_directory.unwrap(),
            verbose: self.verbose,
        })
    }

    fn sanitize_paths(&mut self) -> StdResult<()> {
        let app_dir = std::env::temp_dir().join(format!("signatures-network-node-{}", self.id));

        if self.socket_path.is_none() || self.input_directory.is_none() {
            if app_dir.exists() {
                fs::remove_dir_all(&app_dir).with_context(|| "Previous app dir removal failed")?;
            }
            fs::create_dir_all(&app_dir).with_context(|| "App dir creation failure")?;
        }

        if self.socket_path.is_none() {
            let socket_path = app_dir.join("node.sock");
            if socket_path.exists() {
                fs::remove_file(&socket_path)
                    .with_context(|| "Previous socket file removal failed")?;
            }
            self.socket_path = Some(socket_path);
        }

        if self.input_directory.is_none() {
            let input_dir = app_dir.join("input");
            fs::create_dir(&input_dir).with_context(|| "Input dir creation failure")?;
            self.input_directory = Some(input_dir);
        }

        Ok(())
    }
}

impl SanitizedArgs {
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

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse().sanitize()?;
    let _guard = slog_scope::set_global_logger(build_logger(args.log_level()));

    Application::new(&args.id, &args.socket_path, &args.input_directory)
        .run()
        .await
}

fn build_logger(min_level: Level) -> Logger {
    let drain = slog_bunyan::with_name("signature-network-node", std::io::stdout())
        .set_pretty(false)
        .build()
        .fuse();
    let drain = slog::LevelFilter::new(drain, min_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), o!())
}
