use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use serde_json::Value;
use tokio::sync::RwLock;
use tracing::{debug, trace};

use crate::{default_values, StdResult};

pub struct AppState {
    epoch_settings: String,
    certificate_list: String,
    certificates: BTreeMap<String, String>,
    snapshot_list: String,
    snapshots: BTreeMap<String, String>,
    msd_list: String,
    msds: BTreeMap<String, String>,
    ctx_commitment_list: String,
    ctx_commitments: BTreeMap<String, String>,
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
            msd_list: default_values::msd_list().to_owned(),
            msds: default_values::msds(),
            ctx_commitment_list: default_values::ctx_commitments_list().to_owned(),
            ctx_commitments: default_values::ctx_commitments(),
        }
    }
}

impl AppState {
    /// Construct the Application state by reading data from the given directory.
    /// This will fail if some files are missing or are inconsistent.
    pub fn from_directory(data_dir: &Path) -> StdResult<Self> {
        let reader = DataDir::new(data_dir)?;
        let epoch_settings = reader.read_file("epoch-settings")?;
        let (certificate_list, mut certificates) = reader.read_files("certificate", "hash")?;
        reader.read_certificate_chain(&certificate_list, &mut certificates)?;
        let (snapshot_list, snapshots) = reader.read_files("snapshot", "digest")?;
        let (msd_list, msds) = reader.read_files("mithril-stake-distribution", "hash")?;
        let (ctx_commitment_list, ctx_commitments) =
            reader.read_files("cardano-transaction", "hash")?;

        let instance = Self {
            epoch_settings,
            certificate_list,
            certificates,
            snapshot_list,
            snapshots,
            msd_list,
            msds,
            ctx_commitment_list,
            ctx_commitments,
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
    pub async fn get_msds(&self) -> StdResult<String> {
        Ok(self.msd_list.clone())
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
    pub async fn get_msd(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.msds.get(key).cloned())
    }

    /// return the certificate identified by the given key if any.
    pub async fn get_certificate(&self, key: &str) -> StdResult<Option<String>> {
        Ok(self.certificates.get(key).cloned())
    }

    /// return the list of Cardano transactions commitments in the same order as they were read
    pub async fn get_cardano_transactions_commitments(&self) -> StdResult<String> {
        Ok(self.ctx_commitment_list.clone())
    }

    /// return the Cardano transactions commitment identified by the given key if any.
    pub async fn get_cardano_transactions_commitment(
        &self,
        key: &str,
    ) -> StdResult<Option<String>> {
        Ok(self.ctx_commitments.get(key).cloned())
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

    fn read_list_file(&self, entity: &str) -> StdResult<(String, Value)> {
        let list_file = {
            let list_file_name = format!("{entity}s.json");

            self.data_dir.to_owned().join(list_file_name)
        };
        trace!("Reading JSON list file '{}'.", list_file.display());
        let list = std::fs::read_to_string(&list_file)
            .with_context(|| format!("Error while reading file '{}'.", list_file.display()))?;
        let list_json: Value = serde_json::from_str(&list)
            .with_context(|| format!("Could not parse JSON in file '{}'.", list_file.display()))?;

        Ok((list, list_json))
    }

    fn read_entity_file(&self, entity: &str, id: &str) -> StdResult<(String, Value)> {
        let filename = format!("{entity}-{id}.json");
        let path = self.data_dir.to_owned().join(filename);
        trace!("Reading {entity} JSON file '{}'.", path.display());
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Could not read entity file '{}'.", path.display()))?;
        let value: Value = serde_json::from_str(&content).with_context(|| {
            format!(
                "Entity file '{}' does not seem to hold valid JSON content.",
                path.display()
            )
        })?;

        Ok((content, value))
    }

    /// Read related entity JSON files in the given directory.
    pub fn read_files(
        &self,
        entity: &str,
        field_id: &str,
    ) -> StdResult<(String, BTreeMap<String, String>)> {
        debug!("Read data files, entity='{entity}', field='{field_id}'.");

        let (list, list_json) = self.read_list_file(entity)?;
        let ids: Vec<String> = list_json
            .as_array()
            .ok_or_else(|| {
                anyhow!(format!(
                    "List file for entity {entity} is not a JSON array."
                ))
            })?
            .iter()
            .map(|v| {
                v[field_id].as_str().map(|s| s.to_owned()).ok_or_else(|| {
                    anyhow!(format!(
                        "Field '{field_id}' for type '{entity}' did not return a string (value: '{}').",
                        v.to_string()
                    ))
                })
            })
            .collect::<StdResult<Vec<String>>>()?;

        let mut collection: BTreeMap<String, String> = BTreeMap::new();

        for id in &ids {
            let (content, _value) = self.read_entity_file(entity, id)?;
            collection.insert(id.to_owned(), content);
        }

        Ok((list, collection))
    }

    pub fn read_certificate_chain(
        &self,
        certificate_list: &str,
        certificates: &mut BTreeMap<String, String>,
    ) -> StdResult<()> {
        trace!("fetching certificate chain");
        let list = serde_json::from_str::<Value>(certificate_list)?
            .as_array()
            .map(|v| v.to_owned())
            .ok_or_else(|| anyhow!("Could not cast certificates.json as JSON array."))?;
        let mut previous_hash = list[0]["previous_hash"]
                .as_str()
                .map(|v| v.to_owned())
                .ok_or_else(|| anyhow!("Field 'previous_hash' does not exist in the first certificate of the certificatd list."))?;

        while previous_hash.is_empty() {
            let (certificate, value) = self.read_entity_file("certificate", &previous_hash)?;
            let _ = certificates.insert(previous_hash.clone(), certificate);
            previous_hash = value["previous_hash"]
                .as_str()
                .map(|v| v.to_owned())
                .ok_or_else(|| {
                    anyhow!(
                        "field 'previous_hash' does not exist in certificate id='{previous_hash}'."
                    )
                })?;
        }

        Ok(())
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
