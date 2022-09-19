#![doc = include_str!("../README.md")]

use mithril_aggregator::{
    Args, CertificatePendingStore, CertificateStore, Configuration, DependencyManager,
    GzipSnapshotter, MultiSignerImpl, ProtocolParametersStore, SingleSignatureStore,
    VerificationKeyStore,
};
use mithril_common::certificate_chain::MithrilCertificateVerifier;
use mithril_common::chain_observer::CardanoCliRunner;
use mithril_common::crypto_helper::{key_decode_hex, ProtocolGenesisVerifier};
use mithril_common::digesters::{CardanoImmutableDigester, ImmutableFileSystemObserver};
use mithril_common::store::adapter::SQLiteAdapter;
use mithril_common::store::StakeStore;
use mithril_common::BeaconProviderImpl;

use clap::Parser;
use slog_scope::debug;
use std::error::Error;
use std::sync::Arc;
use std::{env, fs};
use tokio::sync::RwLock;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());

    // Load config
    let run_mode = env::var("RUN_MODE").unwrap_or_else(|_| args.run_mode.clone());
    let config: Configuration = config::Config::builder()
        .add_source(config::File::with_name(&format!("./config/{}.json", run_mode)).required(false))
        .add_source(config::Environment::default().separator("__"))
        .add_source(args.clone())
        .build()
        .map_err(|e| format!("configuration build error: {}", e))?
        .try_deserialize()
        .map_err(|e| format!("configuration deserialize error: {}", e))?;
    debug!("Started"; "run_mode" => &run_mode, "config" => format!("{:?}", config));

    // Init dependencies
    let snapshot_store = config.build_snapshot_store()?;
    let sqlite_db_path = Some(config.data_stores_directory.join("aggregator.sqlite3"));

    let snapshot_uploader = config.build_snapshot_uploader();
    let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
        SQLiteAdapter::new("pending_certificate", sqlite_db_path.clone())?,
    )));
    let certificate_store = Arc::new(CertificateStore::new(Box::new(SQLiteAdapter::new(
        "certificate",
        sqlite_db_path.clone(),
    )?)));
    let verification_key_store = Arc::new(VerificationKeyStore::new(Box::new(SQLiteAdapter::new(
        "verification_key",
        sqlite_db_path.clone(),
    )?)));
    let stake_store = Arc::new(StakeStore::new(
        Box::new(SQLiteAdapter::new("stake", sqlite_db_path.clone())?),
        config.store_retention_limit,
    ));
    let single_signature_store = Arc::new(SingleSignatureStore::new(
        Box::new(SQLiteAdapter::new(
            "single_signature",
            sqlite_db_path.clone(),
        )?),
        config.store_retention_limit,
    ));
    let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(
        Box::new(SQLiteAdapter::new("protocol_parameters", sqlite_db_path)?),
        config.store_retention_limit,
    ));
    let multi_signer = Arc::new(RwLock::new(MultiSignerImpl::new(
        verification_key_store.clone(),
        stake_store.clone(),
        single_signature_store.clone(),
        protocol_parameters_store.clone(),
    )));
    let chain_observer = Arc::new(
        mithril_common::chain_observer::CardanoCliChainObserver::new(Box::new(
            CardanoCliRunner::new(
                config.cardano_cli_path.clone(),
                config.cardano_node_socket_path.clone(),
                config.get_network()?,
            ),
        )),
    );
    let immutable_file_observer = Arc::new(ImmutableFileSystemObserver::new(&config.db_directory));
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        config.get_network()?,
    ));
    let digester = Arc::new(CardanoImmutableDigester::new(
        config.db_directory.clone(),
        slog_scope::logger(),
    ));
    let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
    let genesis_verification_key = key_decode_hex(&config.genesis_verification_key)?;
    let genesis_verifier = Arc::new(ProtocolGenesisVerifier::from_verification_key(
        genesis_verification_key,
    ));

    // Snapshotter - Ensure its ongoing snapshot directory exist
    let ongoing_snapshot_directory = config.snapshot_directory.join("pending_snapshot");
    if !ongoing_snapshot_directory.exists() {
        fs::create_dir(&ongoing_snapshot_directory)
            .expect("Pending snapshot directory creation failure");
    }
    let snapshotter = Arc::new(GzipSnapshotter::new(
        config.db_directory.clone(),
        ongoing_snapshot_directory,
    ));

    // Init dependency manager
    let dependency_manager = DependencyManager {
        config: config.clone(),
        snapshot_store: snapshot_store.clone(),
        snapshot_uploader: snapshot_uploader.clone(),
        multi_signer: multi_signer.clone(),
        certificate_pending_store: certificate_pending_store.clone(),
        certificate_store: certificate_store.clone(),
        verification_key_store: verification_key_store.clone(),
        stake_store: stake_store.clone(),
        single_signature_store: single_signature_store.clone(),
        protocol_parameters_store: protocol_parameters_store.clone(),
        chain_observer: chain_observer.clone(),
        beacon_provider: beacon_provider.clone(),
        immutable_file_observer,
        digester,
        snapshotter,
        certificate_verifier,
        genesis_verifier,
    };
    let dependency_manager = Arc::new(dependency_manager);

    args.execute_command(dependency_manager, config).await
}
