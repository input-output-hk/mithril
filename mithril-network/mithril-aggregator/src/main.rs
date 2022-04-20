#![doc = include_str!("../README.md")]

mod apispec;
mod dependency;
mod entities;
mod errors;
mod fake_data;
mod http_server;
mod snapshot_store;
mod snapshotter;

use clap::Parser;
use log::debug;
use std::fs::File;
use std::path::Path;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::entities::Config;
use crate::http_server::Server;
use crate::snapshot_store::SnapshotStoreHTTPClient;
use crate::snapshotter::Snapshotter;

/// Node args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// Server listening IP
    #[clap(long, default_value = "0.0.0.0")]
    server_ip: String,

    /// Server listening port
    #[clap(long, default_value_t = 8080)]
    server_port: u16,

    /// Verbosity level
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    /// Config file
    #[clap(short, long, default_value = "./config/dev.json")]
    config_file: String,

    /// Snapshot interval, in seconds
    /// Defaults to 4 hours
    #[clap(long, default_value_t = 14400)]
    snapshot_interval: u32,

    /// Directory to snapshot
    #[clap(long, default_value = "/db")]
    db_directory: String,
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
    debug!("{:?}", config);

    // Init dependencies
    let snapshot_storer = Arc::new(RwLock::new(SnapshotStoreHTTPClient::new(
        config.url_snapshot_manifest.clone(),
    )));

    // Init dependecy manager
    let mut dependency_manager = dependency::DependencyManager::new(config);
    dependency_manager.with_snapshot_storer(snapshot_storer.clone());
    let dependency_manager = Arc::new(dependency_manager);

    // Start snapshot uploader
    let handle = tokio::spawn(async move {
        let snapshotter = Snapshotter::new(
            args.snapshot_interval.clone() * 1000,
            args.db_directory.clone(),
        );
        snapshotter.run().await
    });

    // Start REST server
    println!("Starting server...");
    println!("Press Ctrl+C to stop...");
    let shutdown_signal = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install CTRL+C signal handler");
    };
    let http_server = Server::new(args.server_ip, args.server_port, dependency_manager.clone());
    http_server.start(shutdown_signal).await;

    handle.abort();

    println!("Exiting...");
}
