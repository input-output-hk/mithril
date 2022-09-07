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

/*
impl ArgEnum for StoreType {
    fn from_str(input: &str, ignore_case: bool) -> Result<Self, String> {
        let input = if ignore_case {
            input.to_lowercase()
        } else {
            input.to_string()
        };

        match input.as_str() {
            "certificate_pending" => Ok(Self::CertificatePendingStore),
            "certificate" => Ok(Self::CertificateStore),
            "verification_key" => Ok(Self::VerificationKeyStore),
            "stake" => Ok(Self::StakeStore),
            "single_signature" => Ok(Self::SingleSignatureStore),
            "protocol_parameters" => Ok(Self::ProtocolParametersStore),
            "all" => (Ok(Self::All)),
            s => Err(format!("unknown store type '{}'", s)),
        }
    }

    fn to_possible_value<'a>(&self) -> Option<clap::PossibleValue<'a>> {
        match self {
            Self::CertificatePendingStore => Some("certificate_pending"),
            Self::CertificateStore => Some("certificate"),
            Self::VerificationKeyStore => Some("verification_key"),
            Self::StakeStore => Some("stake"),
            Self::SingleSignatureStore => Some("single_signature"),
            Self::ProtocolParametersStore => Some("protocol_parameters"),
            Self::All => Some("all"),
        }
    }

    fn value_variants<'a>() -> &'a [Self] {

    }
}
 */

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
