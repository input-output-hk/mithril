#![doc = include_str!("../README.md")]

mod dependency;
mod entities;
mod http_server;
mod multi_signer;
mod snapshot_store;

use clap::Parser;

use log::debug;
use mithril_common::fake_data;
use mithril_common::snapshotter::Snapshotter;
use std::env;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::entities::Config;
use crate::http_server::Server;
use crate::multi_signer::{
    key_decode_hex, MultiSigner, MultiSignerImpl, ProtocolParameters, ProtocolPartyId,
    ProtocolSignerVerificationKey, ProtocolStake,
};
use crate::snapshot_store::SnapshotStoreHTTPClient;

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

    /// Run mode
    #[clap(short, long, default_value = "dev")]
    run_mode: String,

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
    let run_mode = env::var("RUN_MODE").unwrap_or(args.run_mode);
    debug!("Run Mode: {}", run_mode);
    let config: Config = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default())
        .build()
        .unwrap()
        .try_deserialize()
        .unwrap();
    debug!("{:?}", config);

    // Init dependencies
    let snapshot_storer = Arc::new(RwLock::new(SnapshotStoreHTTPClient::new(
        config.url_snapshot_manifest.clone(),
    )));

    let multi_signer = Arc::new(RwLock::new(init_multi_signer()));

    // Init dependecy manager
    let mut dependency_manager = dependency::DependencyManager::new(config);
    dependency_manager
        .with_snapshot_storer(snapshot_storer.clone())
        .with_multi_signer(multi_signer.clone());
    let dependency_manager = Arc::new(dependency_manager);

    // Start snapshot uploader
    let handle = tokio::spawn(async move {
        let snapshotter =
            Snapshotter::new(args.snapshot_interval * 1000, args.db_directory.clone());
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

/// Init multi signer dependency
fn init_multi_signer() -> impl MultiSigner {
    let mut multi_signer = MultiSignerImpl::new();

    // Update protocol parameters
    let protocol_parameters = fake_data::protocol_parameters();
    let protocol_parameters = ProtocolParameters {
        m: protocol_parameters.m,
        k: protocol_parameters.k,
        phi_f: protocol_parameters.phi_f as f64,
    };
    multi_signer
        .update_protocol_parameters(&protocol_parameters)
        .expect("update protocol parameters failed");

    // Update stake distribution
    let total_signers = 5;
    let stakes = fake_data::signers_with_stakes(total_signers)
        .iter()
        .map(|signer| {
            (
                signer.party_id as ProtocolPartyId,
                signer.stake as ProtocolStake,
            )
        })
        .collect::<_>();
    multi_signer
        .update_stake_distribution(&stakes)
        .expect("stake distribution update failed");

    // Register signers
    fake_data::signers(total_signers).iter().for_each(|signer| {
        multi_signer
            .register_signer(
                signer.party_id as ProtocolPartyId,
                &key_decode_hex::<ProtocolSignerVerificationKey>(&signer.verification_key).unwrap(),
            )
            .expect("register signer failed");
    });

    multi_signer
}
