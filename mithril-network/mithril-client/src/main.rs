#![doc = include_str!("../README.md")]

mod aggregator;
mod aggregator_fake;
mod client;
mod entities;
mod errors;

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use cli_table::{print_stdout, WithTitle};
use log::debug;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;

use crate::aggregator::*;
use crate::client::Client;
use crate::entities::Config;

/// CLI args
#[derive(Parser)]
#[clap(name = "mithril-client")]
#[clap(about = "An implementation of a Mithril Client", long_about = None)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: Commands,

    /// Config file
    #[clap(short, long, default_value = "./config/dev.json")]
    config_file: String,

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
async fn main() {
    // Load args
    let args = Args::parse();

    // Init logger
    env_logger::Builder::new()
        .target(env_logger::Target::Stdout)
        .filter_level(args.verbose.log_level_filter())
        .init();

    // Load config
    let config: Config = {
        let file_handler = File::open(Path::new(&args.config_file));
        let file = match file_handler {
            Err(e) => panic!("{}: {}", errors::OPEN_CONFIG_FILE, e),
            Ok(f) => f,
        };
        match serde_json::from_reader(file) {
            Err(e) => panic!("{}: {}", errors::PARSE_CONFIG_FILE, e),
            Ok(c) => c,
        }
    };
    let config = Arc::new(config);
    debug!("{:?}", config);

    // Init dependencies
    let aggregator_handler = AggregatorHTTPClient::new(config.clone());

    // Init client
    let mut client = Client::new(config.clone());
    client.with_aggregator_handler(aggregator_handler);

    // Execute commands
    match &args.command {
        Commands::List {} => match client.list_snapshots().await {
            Ok(snapshot_list_items) => print_stdout(snapshot_list_items.with_title()).unwrap(),
            Err(err) => pretty_print_error(err),
        },
        Commands::Show { digest } => match client.show_snapshot(digest.to_string()).await {
            Ok(snapshot_field_items) => print_stdout(snapshot_field_items.with_title()).unwrap(),
            Err(err) => pretty_print_error(err),
        },
        Commands::Download {
            digest,
            location_index,
        } => match client
            .download_snapshot(digest.to_string(), *location_index)
            .await
        {
            Ok((from, to)) => println!(
                "Download success {} #{}\nfrom {}\nto {}",
                digest, location_index, from, to
            ),
            Err(err) => pretty_print_error(err),
        },
        Commands::Restore { digest } => {
            client.restore_snapshot(digest.to_string()).await.unwrap();
        }
    }
}

/// Pretty print error
fn pretty_print_error(err: String) {
    println!("An error occurred:");
    println!("{:?}", err);
}
