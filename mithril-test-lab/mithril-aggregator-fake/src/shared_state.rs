use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use tokio::sync::RwLock;
use tracing::{debug, trace};

use crate::{default_values, StdResult};

pub struct AppState {
    epoch_settings: String,
    certificate_list: String,
    certificates: BTreeMap<String, String>,
    snapshot_list: String,
    snapshots: BTreeMap<String, String>,
    mithril_stake_distribution_list: String,
    mithril_stake_distributions: BTreeMap<String, String>,
    cardano_transaction_snapshot_list: String,
    cardano_transaction_snapshots: BTreeMap<String, String>,
    cardano_transaction_proofs: BTreeMap<String, String>,
    cardano_stake_distribution_list: String,
    cardano_stake_distributions: BTreeMap<String, String>,
    cardano_database_snapshot_list: String,
    cardano_database_snapshots: BTreeMap<String, String>,
}

/// Wrapper to access the application state in shared execution.
pub type SharedState = Arc<RwLock<AppState>>;

/// Easy way to embed Application state in an Arc for shared execution.
impl From<AppState> for SharedState {
    fn from(value: AppState) -> Self {
        Arc::new(RwLock::new(value))
    }
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            epoch_settings: default_values::epoch_settings().to_owned(),
            certificate_list: default_values::certificate_list().to_owned(),
            certificates: default_values::certificates(),
            snapshot_list: default_values::snapshot_list().to_owned(),
            snapshots: default_values::snapshots(),
            mithril_stake_distribution_list: default_values::mithril_stake_distribution_list()
                .to_owned(),
            mithril_stake_distributions: default_values::mithril_stake_distributions(),
            cardano_transaction_snapshot_list: default_values::cardano_transaction_snapshots_list()
                .to_owned(),
            cardano_transaction_snapshots: default_values::cardano_transaction_snapshots(),
            cardano_transaction_proofs: default_values::cardano_transaction_proofs(),
            cardano_stake_distribution_list: default_values::cardano_stake_distribution_list()
                .to_owned(),
            cardano_stake_distributions: default_values::cardano_stake_distributions(),
            cardano_database_snapshot_list: default_values::cardano_database_snapshot_list()
                .to_owned(),
            cardano_database_snapshots: default_values::cardano_database_snapshots(),
        }
    }
}

impl AppState {
    /// Construct the Application state by reading data from the given directory.
    /// This will fail if some files are missing or are inconsistent.
    pub fn from_directory(data_dir: &Path) -> StdResult<Self> {
        let reader = DataDir::new(data_dir)?;
        let epoch_settings = reader.read_file("epoch-settings")?;
        let (certificate_list, certificates) = reader.read_files("certificate")?;
        let (snapshot_list, snapshots) = reader.read_files("snapshot")?;
        let (mithril_stake_distribution_list, mithril_stake_distributions) =
            reader.read_files("mithril-stake-distribution")?;
        let (cardano_transaction_snapshot_list, cardano_transaction_snapshots) =
            reader.read_files("ctx-snapshot")?;
        let (_, cardano_transaction_proofs) = reader.read_files("ctx-proof")?;
        let (cardano_stake_distribution_list, cardano_stake_distributions) =
            reader.read_files("cardano-stake-distribution")?;
        let (cardano_database_snapshot_list, cardano_database_snapshots) =
            reader.read_files("cardano-database")?;

        let instance = Self {
            epoch_settings,
            certificate_list,
            certificates,
            snapshot_list,
            snapshots,
            mithril_stake_distribution_list,
            mithril_stake_distributions,
            cardano_transaction_snapshot_list,
            cardano_transaction_snapshots,
            cardano_transaction_proofs,
            cardano_stake_distribution_list,
            cardano_stake_distributions,
            cardano_database_snapshot_list,
            cardano_database_snapshots,
        };

        Ok(instance)
    }

    /// return the compiled epoch settings
    pub async fn get_epoch_settings(&self) -> StdResult<String> {
        Ok(self.epoch_settings.clone())
    }

    /// return the list of snapshots in the same order as they were read
    pub async fn get_snapshots(&self) -> StdResult<String> {
        Ok(self.snapshot_list.clone())
    }

    /// return the list of Mithril stake distributions in the same order as they were read
    pub async fn get_mithril_stake_distributions(&self) -> StdResult<String> {
        Ok(self.mithril_stake_distribution_list.clone())
    }

    /// return the list of certificates in the same order as they were read
    pub async fn get_certificates(&self) -> StdResult<String> {
        Ok(self.certificate_list.clone())
    }

    /// return the snapshot identified by the given key if any.
    pub async fn get_snapshot(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.snapshots.get(key).cloned())
    }

    /// return the Mithril stake distribution identified by the given key if any.
    pub async fn get_mithril_stake_distribution(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.mithril_stake_distributions.get(key).cloned())
    }

    /// return the certificate identified by the given key if any.
    pub async fn get_certificate(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.certificates.get(key).cloned())
    }

    /// return the list of Cardano transactions snapshots in the same order as they were read
    pub async fn get_cardano_transaction_snapshots(&self) -> StdResult<String> {
        Ok(self.cardano_transaction_snapshot_list.clone())
    }

    /// return the Cardano transactions snapshot identified by the given key if any.
    pub async fn get_cardano_transaction_snapshot(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.cardano_transaction_snapshots.get(key).cloned())
    }

    /// return the Cardano transactions proofs from Cardano transaction hashes.
    pub async fn get_cardano_transaction_proofs(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.cardano_transaction_proofs.get(key).cloned())
    }

    /// return the list of Cardano stake distributions in the same order as they were read
    pub async fn get_cardano_stake_distributions(&self) -> StdResult<String> {
        Ok(self.cardano_stake_distribution_list.clone())
    }

    /// return the Cardano stake distribution identified by the given key if any.
    pub async fn get_cardano_stake_distribution(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.cardano_stake_distributions.get(key).cloned())
    }

    /// return the list of Cardano database snapshots in the same order as they were read
    pub async fn get_cardano_database_snapshots(&self) -> StdResult<String> {
        Ok(self.cardano_database_snapshot_list.clone())
    }

    /// return the Cardano database snapshot identified by the given key if any.
    pub async fn get_cardano_database_snapshot(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.cardano_database_snapshots.get(key).cloned())
    }
}

struct DataDir {
    data_dir: PathBuf,
}

impl DataDir {
    /// public constructor
    pub fn new(data_dir: &Path) -> StdResult<Self> {
        if !data_dir.exists() {
            return Err(anyhow!(format!(
                "Path '{}' does not exist.",
                data_dir.display()
            )));
        }

        if !data_dir.is_dir() {
            return Err(anyhow!(format!(
                "Path '{}' is not a directory!",
                data_dir.display()
            )));
        }

        let instance = Self {
            data_dir: data_dir.to_owned(),
        };

        Ok(instance)
    }

    fn read_file(&self, entity: &str) -> StdResult<String> {
        let file = {
            let file_name = format!("{entity}.json");

            self.data_dir.to_owned().join(file_name)
        };

        trace!("Reading JSON file '{}'.", file.display());
        let file_content = std::fs::read_to_string(&file)
            .with_context(|| format!("Error while reading file '{}'.", file.display()))?;

        Ok(file_content)
    }

    fn read_list_file(&self, entity: &str) -> StdResult<String> {
        self.read_file(&format!("{entity}s-list"))
    }

    fn extract_entity_content(
        entity: &str,
        key: &String,
        value: &serde_json::Value,
    ) -> StdResult<(String, String)> {
        let json_content = serde_json::to_string(value)
            .with_context(|| format!("Could not serialize '{entity}-{key}' as JSON."))?;
        Ok((key.to_owned(), json_content))
    }

    fn read_entities_file(&self, entity: &str) -> StdResult<BTreeMap<String, String>> {
        let file_content = self.read_file(&format!("{entity}s"))?;
        let parsed_json: serde_json::Value = serde_json::from_str(&file_content)
            .with_context(|| format!("Could not parse JSON in file '{entity}s.json'."))?;
        let json_object = parsed_json.as_object().with_context(|| {
            format!("Collection file for entity {entity} is not a JSON hashmap.")
        })?;
        let res: Result<Vec<_>, _> = json_object
            .iter()
            .map(|(key, value)| Self::extract_entity_content(entity, key, value))
            .collect();

        Ok(BTreeMap::from_iter(res?))
    }

    /// Read related entity JSON files in the given directory.
    pub fn read_files(&self, entity: &str) -> StdResult<(String, BTreeMap<String, String>)> {
        debug!("Read data files, entity='{entity}'.");

        let list = self.read_list_file(entity)?;
        let collection = self.read_entities_file(entity)?;

        Ok((list, collection))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_appstate_from_default_data() {
        AppState::from_directory(Path::new("./default_data"))
            .expect("Should be able to construct an AppState from the default_data");
    }
}
