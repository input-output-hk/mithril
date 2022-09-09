use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use clap::{Parser, ValueEnum};
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::fmt::Debug;
use std::hash::Hash;

use mithril_common::{
    crypto_helper::ProtocolParameters,
    entities::{
        Beacon, Certificate, CertificatePending, Epoch, PartyId, Signer, SingleSignatures,
        StakeDistribution,
    },
    store::adapter::{JsonFileStoreAdapter, SQLiteAdapter},
    store::adapter_migration::AdapterMigration,
};
type ApplicationResult<T> = Result<T, Box<dyn std::error::Error>>;

async fn migrate(
    base_dir: &Path,
    store_name: &str,
    store_type: StoreType,
) -> ApplicationResult<()> {
    let sqlite_file = base_dir.join("aggregator.sqlite3");
    store_type
        .migrate(&base_dir.join(format!("{}_db", store_name)), &sqlite_file)
        .await
}

async fn migrate_one<
    K: Eq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash + Debug,
    V: Clone + Serialize + DeserializeOwned + Sync + Send,
>(
    source_file: &Path,
    target_file: &Path,
    store_name: &str,
) -> ApplicationResult<()> {
    let source_adapter: JsonFileStoreAdapter<K, V> =
        JsonFileStoreAdapter::new(source_file.to_path_buf())?;
    let target_adapter: SQLiteAdapter<K, V> =
        SQLiteAdapter::new(store_name, Some(target_file.to_path_buf()))?;
    let mut migrator = AdapterMigration::new(source_adapter, target_adapter);

    migrator.migrate().await?;
    let migration_result = migrator.check().await?;

    if migration_result.is_ok() {
        Ok(())
    } else {
        Err(format!("Data consistency check failed: {}", migration_result).into())
    }
}

#[derive(Debug, Clone, Parser)]
enum MigrationCommand {
    #[clap(about = "Automatic aggregator stores migration (preferred).")]
    Automatic(AutomaticMigrationCommand),
    #[clap(about = "Allow store migration one at the time.")]
    Manual(ManualMigrationCommand),
}

impl MigrationCommand {
    pub async fn execute(&self) -> ApplicationResult<()> {
        match self {
            Self::Automatic(command) => command.execute().await,
            Self::Manual(command) => command.execute().await,
        }
    }
}

#[derive(Debug, Clone, Parser)]
struct Args {
    #[clap(subcommand)]
    command: MigrationCommand,
}

#[derive(Debug, Clone, Parser)]
struct ManualMigrationCommand {
    #[clap(long, short, help = "File where source JSON lines are stored.")]
    input_file: PathBuf,

    #[clap(long, short, help = "Destination SQLite format file.")]
    output_file: PathBuf,

    #[clap(value_enum, long, short = 't', help = "What kind of values are there.")]
    store_type: StoreType,
}

impl ManualMigrationCommand {
    pub async fn execute(&self) -> ApplicationResult<()> {
        self.store_type
            .migrate(&self.input_file, &self.output_file)
            .await
    }
}

#[derive(Debug, ValueEnum, Clone)]
enum StoreType {
    CertificatePending,
    Certificate,
    VerificationKey,
    Stake,
    SingleSignature,
    ProtocolParameters,
}

impl StoreType {
    pub async fn migrate(&self, input_file: &Path, output_file: &Path) -> ApplicationResult<()> {
        match self {
            StoreType::CertificatePending => {
                println!("Migrating certificate_pending_store data…");
                migrate_one::<String, CertificatePending>(
                    input_file,
                    output_file,
                    "certificate_pending",
                )
                .await?;
            }
            StoreType::Certificate => {
                println!("Migrating certificate_store data…");
                migrate_one::<String, Certificate>(input_file, output_file, "certificate").await?;
            }

            StoreType::ProtocolParameters => {
                println!("Migrating protocol_parameters_store data…");
                migrate_one::<Epoch, ProtocolParameters>(
                    input_file,
                    output_file,
                    "protocol_parameters",
                )
                .await?;
            }

            StoreType::VerificationKey => {
                println!("Migrating verification_key_store data…");
                migrate_one::<Beacon, HashMap<PartyId, Signer>>(
                    input_file,
                    output_file,
                    "verification_key",
                )
                .await?;
            }
            StoreType::Stake => {
                println!("Migrating stake_store data…");
                migrate_one::<Epoch, StakeDistribution>(input_file, output_file, "stake").await?;
            }

            StoreType::SingleSignature => {
                println!("Migrating single_signature_store data…");
                migrate_one::<Beacon, HashMap<PartyId, SingleSignatures>>(
                    input_file,
                    output_file,
                    "single_signature",
                )
                .await?;
            }
        }
        println!("OK ✓");

        Ok(())
    }
}

#[derive(Debug, Clone, Parser)]
struct AutomaticMigrationCommand {
    #[clap(long, short, env = "MITHRIL_STORE_DIR")]
    db_dir: PathBuf,
}

impl AutomaticMigrationCommand {
    pub async fn execute(&self) -> ApplicationResult<()> {
        if !self.db_dir.exists() {
            return Err(format!(
                "Base store directory '{}' does not exist… quitting!",
                self.db_dir.display()
            )
            .into());
        }
        let base_dir = &self.db_dir;

        migrate(base_dir, "certificate", StoreType::Certificate).await?;
        migrate(
            base_dir,
            "pending_certificate",
            StoreType::CertificatePending,
        )
        .await?;
        migrate(
            base_dir,
            "protocol_parameters",
            StoreType::ProtocolParameters,
        )
        .await?;
        migrate(base_dir, "single_signature", StoreType::SingleSignature).await?;
        migrate(base_dir, "verification_key", StoreType::VerificationKey).await?;
        migrate(base_dir, "stake", StoreType::Stake).await?;

        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    println!("Mithril Aggregator JSON → SQLite migration tool.");

    args.command.execute().await
}
