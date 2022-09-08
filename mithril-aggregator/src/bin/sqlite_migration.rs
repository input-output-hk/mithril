use std::{collections::HashMap, error::Error, path::PathBuf};

use clap::{Parser, ValueEnum};

use mithril_common::{
    crypto_helper::ProtocolParameters,
    entities::{
        Beacon, Certificate, CertificatePending, Epoch, PartyId, Signer, SingleSignatures,
        StakeDistribution,
    },
    store::adapter::{JsonFileStoreAdapter, SQLiteAdapter},
    store::adapter_migration::AdapterMigration,
};

#[derive(Parser, Debug)]
struct Args {
    #[clap(long, short, help = "File where source JSON lines are stored.")]
    input_file: PathBuf,

    #[clap(long, short, help = "Destination SQLite format file.")]
    output_file: PathBuf,

    #[clap(value_enum, long, short = 't', help = "What kind of values are there.")]
    store_type: StoreType,
}

#[derive(Debug, ValueEnum, Clone)]
enum StoreType {
    CertificatePendingStore,
    CertificateStore,
    VerificationKeyStore,
    StakeStore,
    SingleSignatureStore,
    ProtocolParametersStore,
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    match args.store_type {
        StoreType::CertificatePendingStore => {
            let mut migration: AdapterMigration<String, CertificatePending> = AdapterMigration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("certificate_pending", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::CertificateStore => {
            let mut migration: AdapterMigration<String, Certificate> = AdapterMigration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("certificate", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::ProtocolParametersStore => {
            let mut migration: AdapterMigration<Epoch, ProtocolParameters> = AdapterMigration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("protocol_parameters", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::VerificationKeyStore => {
            let mut migration: AdapterMigration<Epoch, HashMap<PartyId, Signer>> =
                AdapterMigration::new(
                    JsonFileStoreAdapter::new(args.input_file)?,
                    SQLiteAdapter::new("verification_key", Some(args.output_file))?,
                );
            migration.migrate().await
        }
        StoreType::StakeStore => {
            let mut migration: AdapterMigration<Epoch, StakeDistribution> = AdapterMigration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("stake_distribution", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::SingleSignatureStore => {
            let mut migration: AdapterMigration<Beacon, HashMap<PartyId, SingleSignatures>> =
                AdapterMigration::new(
                    JsonFileStoreAdapter::new(args.input_file)?,
                    SQLiteAdapter::new("single_signature", Some(args.output_file))?,
                );
            migration.migrate().await
        }
    }
}
