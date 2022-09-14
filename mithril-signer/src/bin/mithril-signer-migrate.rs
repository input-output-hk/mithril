use clap::{Parser, ValueEnum};
use mithril_common::crypto_helper::ProtocolInitializer;
use mithril_common::entities::{Epoch, StakeDistribution};
use serde::{de::DeserializeOwned, Serialize};

use std::fmt::Debug;
use std::hash::Hash;
use std::path::{Path, PathBuf};

use mithril_common::store::adapter::{JsonFileStoreAdapter, SQLiteAdapter};
use mithril_common::store::adapter_migration::AdapterMigration;

type ApplicationResult<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Debug, Clone, Parser)]
enum MigrationCommand {
    #[clap(about = "Automatic aggregator stores migration (preferred).")]
    Automatic(AutomaticMigrationCommand),
    #[clap(about = "Allow store migration one at the time.")]
    Manual(ManualMigrationCommand),
}

impl MigrationCommand {
    /// Launch migration command.
    pub async fn execute(&self) -> ApplicationResult<()> {
        match self {
            Self::Automatic(command) => command.execute().await,
            Self::Manual(command) => command.execute().await,
        }
    }
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

        migrate(
            base_dir,
            "protocol_initializer",
            StoreType::ProtocolInitializer,
        )
        .await?;
        migrate(base_dir, "stake", StoreType::Stake).await?;

        Ok(())
    }
}

#[derive(Debug, Clone, ValueEnum)]
enum StoreType {
    Stake,
    ProtocolInitializer,
}

impl StoreType {
    pub async fn migrate(&self, input_file: &Path, output_file: &Path) -> ApplicationResult<()> {
        match self {
            Self::Stake => {
                println!("Migrating stake_store data…");
                migrate_one::<Epoch, StakeDistribution>(
                    input_file,
                    output_file,
                    "stake",
                    |right, left| right.eq(&left),
                )
                .await?;
            }
            Self::ProtocolInitializer => {
                println!("Migrating protocol_initializer_store data…");
                migrate_one::<Epoch, ProtocolInitializer>(
                    input_file,
                    output_file,
                    "protocol_initializer",
                    |_right, _left| true,
                )
                .await?;
            }
        };
        println!("OK ✓");

        Ok(())
    }
}

#[derive(Debug, Clone, Parser)]
struct Args {
    #[clap(subcommand)]
    command: MigrationCommand,
}
async fn migrate(
    base_dir: &Path,
    store_name: &str,
    store_type: StoreType,
) -> ApplicationResult<()> {
    let sqlite_file = base_dir.join("signer.sqlite3");
    store_type
        .migrate(&base_dir.join(format!("{}_db", store_name)), &sqlite_file)
        .await
}

async fn migrate_one<K, V>(
    source_file: &Path,
    target_file: &Path,
    store_name: &str,
    checker: fn((&K, &V), (&K, &V)) -> bool,
) -> ApplicationResult<()>
where
    K: Eq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash + Debug,
    V: Clone + Serialize + DeserializeOwned + Sync + Send,
{
    let source_adapter: JsonFileStoreAdapter<K, V> =
        JsonFileStoreAdapter::new(source_file.to_path_buf())?;
    let target_adapter: SQLiteAdapter<K, V> =
        SQLiteAdapter::new(store_name, Some(target_file.to_path_buf()))?;
    let mut migrator = AdapterMigration::new(source_adapter, target_adapter);

    migrator.migrate().await?;
    let migration_result = migrator.check(checker).await?;

    if migration_result.is_ok() {
        Ok(())
    } else {
        Err(format!("Data consistency check failed: {}", migration_result).into())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    println!("Mithril Signer JSON → SQLite migration tool.");

    args.command.execute().await
}
