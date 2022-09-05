#![doc = include_str!("../README.md")]

use clap::{Parser, Subcommand};
use cli_table::{print_stdout, WithTitle};
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::env;
use std::error::Error;
use std::sync::Arc;

use mithril_client::{convert_to_field_items, AggregatorHTTPClient, Config, Runtime};

use mithril_common::certificate_chain::MithrilCertificateVerifier;

/// CLI args
#[derive(Parser)]
#[clap(name = "mithril-client")]
#[clap(about = "An implementation of a Mithril Client", long_about = None)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: Commands,

    /// Run Mode
    #[clap(short, long, default_value = "dev")]
    run_mode: String,

    /// Verbosity level
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,
}

impl Args {
    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn build_logger(&self) -> Logger {
        let decorator = slog_term::PlainDecorator::new(std::io::stdout());
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }
}

/// CLI command list
#[derive(Subcommand)]
enum Commands {
    /// List available snapshots
    #[clap(arg_required_else_help = false)]
    List {
        /// JSON output mode
        #[clap(long)]
        json: bool,
    },

    /// Infos about a snapshot
    #[clap(arg_required_else_help = false)]
    Show {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,

        /// JSON output mode
        #[clap(long)]
        json: bool,
    },

    /// Download a snapshot
    #[clap(arg_required_else_help = true)]
    Download {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,

        /// Snapshot location index
        #[clap(required = false, default_value_t = 1)]
        location_index: isize,
    },

    /// Restore a snapshot
    #[clap(arg_required_else_help = true)]
    Restore {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,
    },
}

#[tokio::main]
async fn main() -> Result<(), String> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or(args.run_mode);
    debug!("Run Mode: {}", run_mode);
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .build()
        .map_err(|e| format!("configuration build error: {}", e))?
        .try_deserialize()
        .map_err(|e| format!("configuration deserialize error: {}", e))?;
    debug!("{:?}", config);

    // Init dependencies
    let aggregator_handler = Arc::new(AggregatorHTTPClient::new(
        config.network.clone(),
        config.aggregator_endpoint.clone(),
    ));
    let verifier = Box::new(MithrilCertificateVerifier::new(slog_scope::logger()));

    // Init runtime
    let mut runtime = Runtime::new(config.network.clone());
    runtime
        .with_aggregator_handler(aggregator_handler)
        .with_verifier(verifier);

    // Execute commands
    match &args.command {
        Commands::List { json } => match runtime.list_snapshots().await {
            Ok(snapshot_list_items) => {
                if *json {
                    println!("{}", serde_json::to_string(&snapshot_list_items).unwrap());
                } else {
                    print_stdout(snapshot_list_items.with_title()).unwrap();
                }
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Show { digest, json } => match runtime.show_snapshot(digest).await {
            Ok(snapshot_field_items) => {
                if *json {
                    println!("{}", serde_json::to_string(&snapshot_field_items).unwrap());
                } else {
                    print_stdout(
                        convert_to_field_items(&snapshot_field_items, config.network.clone())
                            .with_title(),
                    )
                    .unwrap();
                }
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Download {
            digest,
            location_index,
        } => match runtime.download_snapshot(digest, *location_index).await {
            Ok((from, to)) => {
                println!(
                    "Download success {} #{}\nfrom {}\nto {}",
                    digest, location_index, from, to
                );
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
        Commands::Restore { digest } => match runtime.restore_snapshot(digest).await {
            Ok(to) => {
                println!(
                    r###"Unpack success {}
to {}

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} inputoutput/cardano-node

"###,
                    digest,
                    to,
                    to,
                    config.network.clone()
                );
                Ok(())
            }
            Err(err) => pretty_print_error(err),
        },
    }
}

/// Pretty print error
fn pretty_print_error(err: impl Error) -> Result<(), String> {
    let message = format!("An error occurred: {}", err);

    Err(message)
}
