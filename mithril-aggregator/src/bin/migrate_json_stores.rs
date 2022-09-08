use std::{collections::HashMap, path::PathBuf};

use clap::Parser;
use serde::de::DeserializeOwned;
use serde::Serialize;
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

async fn migrate<
    K: PartialEq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash,
    V: Clone + Serialize + DeserializeOwned + Sync + Send,
>(
    base_dir: &PathBuf,
    store_name: &str,
) -> ApplicationResult<()> {
    let sqlite_file = base_dir.join("aggregator.sqlite3");
    let source_adapter: JsonFileStoreAdapter<K, V> =
        JsonFileStoreAdapter::new(base_dir.join(format!("{}_db", store_name)))?;
    let target_adapter: SQLiteAdapter<K, V> =
        SQLiteAdapter::new(store_name, Some(sqlite_file.clone()))?;
    let mut migrator = AdapterMigration::new(source_adapter, target_adapter);

    migrator.migrate().await?;

    if migrator.check().await? {
        Ok(())
    } else {
        Err(format!("Data consistency check failed!").into())
    }
}

#[derive(Debug, Clone, Parser)]
struct Args {
    #[clap(long, short, env = "MITHRIL_STORE_DIR")]
    db_dir: PathBuf,
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    println!("Mithril Aggregator JSON → SQLite migration tool.");

    if !args.db_dir.exists() {
        return Err(format!(
            "Base store directory '{}' does not exist… quitting!",
            args.db_dir.display()
        )
        .into());
    }
    let base_dir = args.db_dir;

    println!("Migrating certificate_store data…");
    migrate::<String, Certificate>(&base_dir, "certificate").await?;
    println!("OK ✓");

    println!("Migrating pending_certificate_store data…");
    migrate::<String, CertificatePending>(&base_dir, "pending_certificate").await?;
    println!("OK ✓");

    println!("Migrating protocol_parameters_store data…");
    migrate::<Epoch, ProtocolParameters>(&base_dir, "protocol_parameters").await?;
    println!("OK ✓");

    println!("Migrating single_signature_store data…");
    migrate::<Beacon, HashMap<PartyId, SingleSignatures>>(&base_dir, "single_signature").await?;
    println!("OK ✓");

    println!("Migrating verification_key_store data…");
    migrate::<Beacon, HashMap<PartyId, Signer>>(&base_dir, "verification_key").await?;
    println!("OK ✓");

    println!("Migrating stake_store data…");
    migrate::<Epoch, StakeDistribution>(&base_dir, "stake").await?;
    println!("OK ✓");

    Ok(())
}
