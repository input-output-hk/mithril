use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use clap::Parser;
use slog::{debug, o, Drain, Level, Logger};

use mithril_common::StdResult;

use signature_network_node::Application;

#[derive(Parser, Debug, Clone)]
pub struct Args {
    #[clap(long, env)]
    id: String,

    /// Path to the socket file to communicate with this node.
    ///
    /// If given the host directory must exist and existing socket file will be removed.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/signatures-network-node-{id}/node.sock`
    #[clap(long, env)]
    socket_path: Option<PathBuf>,

    /// Path to the directory where the node will listen for new messages.
    ///
    /// If given the directory will be created if it does not exist, if it exists files
    /// won't be removed.
    ///
    /// Optional: if not set it will default to `{system_temp_folder}/signatures-network-node-{id}/input/`
    #[clap(long, env)]
    input_directory: Option<PathBuf>,

    /// Paths to the peer's input directories.
    ///
    /// Messages received from this node socket will be forwarded to these directories.
    #[clap(short, long, value_delimiter = ' ', env)]
    peers_input_directories: Vec<PathBuf>,

    /// Log messages in JSON format
    #[clap(long, env)]
    json_log: bool,

    /// Verbosity level
    #[clap(
        short,
        long,
        action = clap::ArgAction::Count,
        help = "Verbosity level, add more v to increase"
    )]
    verbose: u8,
}

#[derive(Debug)]
struct SanitizedArgs {
    id: String,
    socket_path: PathBuf,
    input_directory: PathBuf,
    peers_input_directories: Vec<PathBuf>,
    log_level: Level,
    enable_json_log: bool,
}

impl Args {
    fn sanitize(mut self) -> StdResult<SanitizedArgs> {
        self.sanitize_paths()?;
        let log_level = self.log_level();

        let sanitized_args = SanitizedArgs {
            id: self.id,
            socket_path: self.socket_path.unwrap(),
            input_directory: self.input_directory.unwrap(),
            peers_input_directories: self.peers_input_directories,
            log_level,
            enable_json_log: self.json_log,
        };

        // Previous socket file must be removed in order for the warp http server to bind to it
        if sanitized_args.socket_path.exists() {
            fs::remove_file(&sanitized_args.socket_path)
                .with_context(|| "Previous socket file removal failed")?;
        }

        if !sanitized_args.input_directory.exists() {
            fs::create_dir(&sanitized_args.input_directory)
                .with_context(|| "Input dir creation failure")?;
        }

        Ok(sanitized_args)
    }

    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
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
            self.socket_path = Some(app_dir.join("node.sock"));
        }

        if self.input_directory.is_none() {
            self.input_directory = Some(app_dir.join("input"));
        }

        Ok(())
    }
}

impl SanitizedArgs {
    fn build_logger(&self) -> Logger {
        if self.enable_json_log {
            let drain = slog_bunyan::with_name("signature-network-node", std::io::stdout())
                .set_pretty(false)
                .build()
                .fuse();
            let drain = slog::LevelFilter::new(drain, self.log_level).fuse();
            let drain = slog_async::Async::new(drain).build().fuse();

            Logger::root(Arc::new(drain), o!())
        } else {
            let decorator = slog_term::TermDecorator::new().build();
            let drain = slog_term::CompactFormat::new(decorator).build().fuse();
            let drain = slog::LevelFilter::new(drain, self.log_level).fuse();
            let drain = slog_async::Async::new(drain).build().fuse();

            Logger::root(Arc::new(drain), o!())
        }
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let args = Args::parse().sanitize()?;
    let logger = args.build_logger();
    debug!(logger, "Starting"; "args" => #?args);

    Application::new(
        &args.id,
        &args.socket_path,
        &args.input_directory,
        args.peers_input_directories,
        &logger,
    )
    .run()
    .await
}
