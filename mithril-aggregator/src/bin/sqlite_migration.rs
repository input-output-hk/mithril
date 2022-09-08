use std::{collections::HashMap, error::Error, hash::Hash, path::PathBuf};

use clap::{Parser, ValueEnum};

use mithril_common::{
    crypto_helper::ProtocolParameters,
    entities::{
        Beacon, Certificate, CertificatePending, Epoch, PartyId, Signer, SingleSignatures,
        StakeDistribution,
    },
    store::adapter::{JsonFileStoreAdapter, SQLiteAdapter, StoreAdapter},
};
use serde::{de::DeserializeOwned, Serialize};

struct Migration<K, V> {
    adapter_from: JsonFileStoreAdapter<K, V>,
    adapter_to: SQLiteAdapter<K, V>,
}

impl<K, V> Migration<K, V>
where
    K: PartialEq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash,
    V: Clone + Serialize + DeserializeOwned + Sync + Send,
{
    pub fn new(adapter_from: JsonFileStoreAdapter<K, V>, adapter_to: SQLiteAdapter<K, V>) -> Self {
        Self {
            adapter_from,
            adapter_to,
        }
    }

    pub async fn migrate(&mut self) -> Result<(), Box<dyn Error>> {
        for (key, record) in self
            .adapter_from
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .rev()
        {
            self.adapter_to.store_record(&key, &record).await?;
        }

        Ok(())
    }
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

#[derive(Parser, Debug)]
struct Args {
    #[clap(long, short, help = "File where source JSON lines are stored.")]
    input_file: PathBuf,

    #[clap(long, short, help = "Destination SQLite format file.")]
    output_file: PathBuf,

    #[clap(value_enum, long, short = 't', help = "What kind of values are there.")]
    store_type: StoreType,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    match args.store_type {
        StoreType::CertificatePendingStore => {
            let mut migration: Migration<String, CertificatePending> = Migration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("certificate_pending", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::CertificateStore => {
            let mut migration: Migration<String, Certificate> = Migration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("certificate", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::ProtocolParametersStore => {
            let mut migration: Migration<Epoch, ProtocolParameters> = Migration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("protocol_parameters", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::VerificationKeyStore => {
            let mut migration: Migration<Epoch, HashMap<PartyId, Signer>> = Migration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("verification_key", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::StakeStore => {
            let mut migration: Migration<Epoch, StakeDistribution> = Migration::new(
                JsonFileStoreAdapter::new(args.input_file)?,
                SQLiteAdapter::new("stake_distribution", Some(args.output_file))?,
            );
            migration.migrate().await
        }
        StoreType::SingleSignatureStore => {
            let mut migration: Migration<Beacon, HashMap<PartyId, SingleSignatures>> =
                Migration::new(
                    JsonFileStoreAdapter::new(args.input_file)?,
                    SQLiteAdapter::new("single_signature", Some(args.output_file))?,
                );
            migration.migrate().await
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, time::Duration};

    use tokio::time::sleep;

    use super::*;

    fn get_adapter(dir_name: &str) -> (PathBuf, JsonFileStoreAdapter<u64, String>) {
        let dir = std::env::temp_dir().join("mithril_test").join(dir_name);

        if dir.exists() {
            let _ = fs::remove_dir_all(&dir);
        }

        (dir.clone(), JsonFileStoreAdapter::new(dir).unwrap())
    }

    #[tokio::test]
    async fn test_migration() {
        let (dir, mut json_adapter) = get_adapter("test_sqlite_migration");
        let sqlite_file = dir.clone().join("test.sqlite3");
        let expected = vec![
            (1_u64, "one".to_string()),
            (2, "two".to_string()),
            (3, "three".to_string()),
            (4, "four".to_string()),
        ];

        for (key, record) in expected.iter() {
            json_adapter.store_record(key, record).await.unwrap();
            sleep(Duration::from_millis(100)).await;
        }
        {
            let sqlite_adapter =
                SQLiteAdapter::new("test_migration", Some(sqlite_file.clone())).unwrap();
            let mut migration = Migration::new(json_adapter, sqlite_adapter);
            migration.migrate().await.unwrap();
        }

        let sqlite_adapter: SQLiteAdapter<u64, String> =
            SQLiteAdapter::new("test_migration", Some(sqlite_file.clone())).unwrap();
        let collected: Vec<(u64, String)> = sqlite_adapter
            .get_last_n_records(usize::MAX)
            .await
            .unwrap()
            .into_iter()
            .rev()
            .collect();

        assert_eq!(expected, collected);
    }
}
