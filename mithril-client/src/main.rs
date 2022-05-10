#![doc = include_str!("../README.md")]

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use cli_table::{print_stdout, WithTitle};
use log::debug;
use std::env;

use mithril_client::{AggregatorHTTPClient, Client, Config, VerifierImpl};

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
    #[clap(flatten)]
    verbose: Verbosity<InfoLevel>,
}

/// CLI command list
#[derive(Subcommand)]
enum Commands {
    /// List available snapshots
    #[clap(arg_required_else_help = false)]
    List {},

    /// Infos about a snapshot
    #[clap(arg_required_else_help = false)]
    Show {
        /// Snapshot digest
        #[clap(required = true)]
        digest: String,
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

    // Init logger
    env_logger::Builder::new()
        .target(env_logger::Target::Stdout)
        .filter_level(args.verbose.log_level_filter())
        .init();

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
    let aggregator_handler = Box::new(AggregatorHTTPClient::new(
        config.network.clone(),
        config.aggregator_endpoint.clone(),
    ));
    let verifier = Box::new(VerifierImpl::new());

    // Init client
    let mut client = Client::new(config.network.clone());
    client
        .with_aggregator_handler(aggregator_handler)
        .with_verifier(verifier);

    // Execute commands
    match &args.command {
        Commands::List {} => match client.list_snapshots().await {
            Ok(snapshot_list_items) => print_stdout(snapshot_list_items.with_title()).unwrap(),
            Err(err) => pretty_print_error(err),
        },
        Commands::Show { digest } => match client.show_snapshot(digest).await {
            Ok(snapshot_field_items) => print_stdout(snapshot_field_items.with_title()).unwrap(),
            Err(err) => pretty_print_error(err),
        },
        Commands::Download {
            digest,
            location_index,
        } => match client.download_snapshot(digest, *location_index).await {
            Ok((from, to)) => println!(
                "Download success {} #{}\nfrom {}\nto {}",
                digest, location_index, from, to
            ),
            Err(err) => pretty_print_error(err),
        },
        Commands::Restore { digest } => match client.restore_snapshot(digest).await {
            Ok(to) => {
                println!(
                    r###"Unpack success {}
to {}

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK=testnet inputoutput/cardano-node

"###,
                    digest, to, to
                )
            }
            Err(err) => pretty_print_error(err),
        },
    }
    Ok(())
}

/// Pretty print error
fn pretty_print_error(err: impl std::error::Error) {
    println!("An error occurred:");
    println!("{}", err);
}
