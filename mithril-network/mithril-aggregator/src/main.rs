#![doc = include_str!("../README.md")]

mod apispec;
mod entities;
mod fake_data;
mod http_server;
mod snapshot_store;
mod snapshotter;

use clap::Parser;

use crate::http_server::Server;
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

    println!("Starting server...");
    println!("Press Ctrl+C to stop...");
    let shutdown_signal = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install CTRL+C signal handler");
    };

    // Start snapshot uploader
    let handle = tokio::spawn(async move {
        let snapshotter = Snapshotter::new(
            args.snapshot_interval.clone() * 1000,
            args.db_directory.clone(),
        );
        snapshotter.run().await
    });

    // Start REST server
    let http_server = Server::new(args.server_ip, args.server_port);
    http_server.start(shutdown_signal).await;

    handle.abort();

    println!("Exiting...");
}
