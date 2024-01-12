#[path = "default_values.rs"]
mod default_values;

use std::{collections::BTreeMap, path::Path, sync::Arc};

use anyhow::{anyhow, Context};
use serde_json::Value;
use tokio::sync::RwLock;
use tracing::{debug, trace};

use crate::StdResult;

pub struct AppState {
    epoch_settings: String,
    certificate_list: String,
    certificates: BTreeMap<String, String>,
    snapshot_list: String,
    snapshots: BTreeMap<String, String>,
    msd_list: String,
    msds: BTreeMap<String, String>,
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
        }
    }
}

impl AppState {
    /// Construct the Application state by reading data from the given directory.
    /// This will fail if some files are missing or are inconsistent.
    pub fn from_directory(data_dir: &Path) -> StdResult<Self> {
        let epoch_settings = default_values::epoch_settings().to_owned();
        let (certificate_list, certificates) = read_files(data_dir, "certificate", "hash")?;
        let (snapshot_list, snapshots) = read_files(data_dir, "snapshot", "digest")?;
        let (msd_list, msds) = read_files(data_dir, "mithril-stake-distribution", "hash")?;

        let instance = Self {
            epoch_settings,
            certificate_list,
            certificates,
            snapshot_list,
            snapshots,
            msd_list,
            msds,
        };

        Ok(instance)
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

    /// return the compiled epoch settings
    pub async fn get_epoch_settings(&self) -> StdResult<String> {
        Ok(self.epoch_settings.clone())
    }
}

/// Read related entity JSON files in the given directory.
fn read_files(
    data_dir: &Path,
    entity: &str,
    field_id: &str,
) -> StdResult<(String, BTreeMap<String, String>)> {
    debug!("Read data files, entity='{entity}', field='{field_id}'.");

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
    let list_file = {
        let list_file_name = format!("{entity}s.json");

        data_dir.to_owned().join(list_file_name)
    };
    trace!("Reading JSON list file '{}'.", list_file.display());
    let list = std::fs::read_to_string(&list_file)
        .with_context(|| format!("Error while reading file '{}'.", list_file.display()))?;
    let list_json: Value = serde_json::from_str(&list)
        .with_context(|| format!("Could not parse JSON in file '{}'.", list_file.display()))?;
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

    for id in ids {
        let filename = format!("{entity}-{id}.json");
        let path = data_dir.to_owned().join(&filename);
        trace!("Reading {entity} JSON file '{}'.", path.display());
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Could not read entity file '{}'.", path.display()))?;
        let _value: Value = serde_json::from_str(&content).with_context(|| {
            format!(
                "Entity file '{}' does not seem to hold valid JSON content.",
                path.display()
            )
        })?;
        collection.insert(id, content);
    }

    Ok((list, collection))
}
