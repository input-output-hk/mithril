use std::{collections::BTreeMap, sync::Arc};

use anyhow::{anyhow, Context};
use serde_json::Value;
use tokio::sync::RwLock;

use crate::StdResult;

fn epoch_settings() -> &'static str {
    r#"{"epoch":112,"protocol":{"k":5,"m":100,"phi_f":0.65},"next_protocol":{"k":5,"m":100,"phi_f":0.65}}"#
}

fn snapshots() -> &'static str {
    r#"[{"digest":"28a9362c74b8739215b05c51ec856cef76b529d4c3f0bbde7dd6cba2ca1f3a20","beacon":{"network":"preprod","epoch":111,"immutable_file_number":2153},"certificate_hash":"98c1ff67698c9c8a655f70455daaa95ac0463b6c2b588af5fc4d4e5000093cc3","size":1457951050,"created_at":"2023-12-11T02:46:44.065578795Z","locations":["https://storage.googleapis.com/cdn.aggregator.release-preprod.api.mithril.network/preprod-e111-i2153.28a9362c74b8739215b05c51ec856cef76b529d4c3f0bbde7dd6cba2ca1f3a20.tar.zst"],"compression_algorithm":"zstandard","cardano_node_version":"8.1.2"},{"digest":"09644434c25e34ddee8e0e3d8db777fd43adbc8073f5467b0a8deb7508dee11c","beacon":{"network":"preprod","epoch":111,"immutable_file_number":2152},"certificate_hash":"4cff6b721596ecc8ff2fdcbd9c6a57c1bb8ed0af27747c961f36d492c103413a","size":1457400203,"created_at":"2023-12-10T20:37:49.924409981Z","locations":["https://storage.googleapis.com/cdn.aggregator.release-preprod.api.mithril.network/preprod-e111-i2152.09644434c25e34ddee8e0e3d8db777fd43adbc8073f5467b0a8deb7508dee11c.tar.zst"],"compression_algorithm":"zstandard","cardano_node_version":"8.1.2"}]"#
}

fn certificates() -> &'static str {
    r#"[{"hash":"e7e0ed069ec666200dd9c8879cdbfba4e6f4d12f5e638a818028771eec9ecff8","previous_hash":"e47cb4ff4caeecbbe01e271a401b6292aa0a74891434b771e2ee87eb8e00227a","beacon":{"network":"preprod","epoch":112,"immutable_file_number":2171},"metadata":{"version":"0.1.0","parameters":{"k":5,"m":100,"phi_f":0.65},"initiated_at":"2023-12-15T14:50:01.404144312Z","sealed_at":"2023-12-15T14:51:01.949417597Z","total_signers":3},"protocol_message":{"message_parts":{"snapshot_digest":"5fa6d76d77583ee2eb2a70fe3791a2d34be5437cfa37fc860738cdb48c23dad1","next_aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b39302c3230312c3139332c3133312c32382c34342c3235302c31312c3136382c32312c3232392c3132332c35382c36392c33352c3230382c3234342c3235322c36392c3235312c36312c3232392c3230372c3232302c372c3233352c37322c3233322c31322c3136302c32322c35335d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373136373237373137303538397d"}},"signed_message":"253c9da1fc16866df0fe4340032014983fdd349e93a13d9059d2ca659d98c018","aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b33382c31392c3234382c3138312c39352c3131332c3233362c3231392c3138362c3234362c3132302c3132362c3139302c372c332c33312c302c3130382c36302c39302c3138392c3232382c312c33312c38342c3233352c3131372c35312c3130362c3137362c38342c31315d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373036303931323638363639397d"},{"hash":"c3965e9541261c7bdf32f2505ef3b8e57cec9b875c4f1b4627a068b1c7c90f62","previous_hash":"e47cb4ff4caeecbbe01e271a401b6292aa0a74891434b771e2ee87eb8e00227a","beacon":{"network":"preprod","epoch":112,"immutable_file_number":2170},"metadata":{"version":"0.1.0","parameters":{"k":5,"m":100,"phi_f":0.65},"initiated_at":"2023-12-15T08:26:05.508401086Z","sealed_at":"2023-12-15T08:27:06.369667490Z","total_signers":21},"protocol_message":{"message_parts":{"snapshot_digest":"593a95cee76541823a6a67b8b4d918006d767896c1a5da27a64efa3eb3f0c296","next_aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b39302c3230312c3139332c3133312c32382c34342c3235302c31312c3136382c32312c3232392c3132332c35382c36392c33352c3230382c3234342c3235322c36392c3235312c36312c3232392c3230372c3232302c372c3233352c37322c3233322c31322c3136302c32322c35335d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373136373237373137303538397d"}},"signed_message":"c9ba0869e99fcd28abe0ee5fb4a782614e27a3279290218a268f590931f30975","aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b33382c31392c3234382c3138312c39352c3131332c3233362c3231392c3138362c3234362c3132302c3132362c3139302c372c332c33312c302c3130382c36302c39302c3138392c3232382c312c33312c38342c3233352c3131372c35312c3130362c3137362c38342c31315d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373036303931323638363639397d"},{"hash":"68548cbeab469624a66c5735367e317f504e2532acbe65ad857fb8ad1357e7db","previous_hash":"e47cb4ff4caeecbbe01e271a401b6292aa0a74891434b771e2ee87eb8e00227a","beacon":{"network":"preprod","epoch":112,"immutable_file_number":2169},"metadata":{"version":"0.1.0","parameters":{"k":5,"m":100,"phi_f":0.65},"initiated_at":"2023-12-15T02:47:37.548155038Z","sealed_at":"2023-12-15T02:48:37.758635033Z","total_signers":4},"protocol_message":{"message_parts":{"snapshot_digest":"73994321fa86eaedd27387532f89591cac8f089350e7fcf1a89f6e0ffb47293d","next_aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b39302c3230312c3139332c3133312c32382c34342c3235302c31312c3136382c32312c3232392c3132332c35382c36392c33352c3230382c3234342c3235322c36392c3235312c36312c3232392c3230372c3232302c372c3233352c37322c3233322c31322c3136302c32322c35335d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373136373237373137303538397d"}},"signed_message":"b09356a9fb2a58cf90a8b5ba8cb03b6c83c7e70cc8a25a6b9b773e3a44c51376","aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b33382c31392c3234382c3138312c39352c3131332c3233362c3231392c3138362c3234362c3132302c3132362c3139302c372c332c33312c302c3130382c36302c39302c3138392c3232382c312c33312c38342c3233352c3131372c35312c3130362c3137362c38342c31315d2c226e725f6c6561766573223a32382c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a35373036303931323638363639397d"}]"#
}

fn msds() -> &'static str {
    r#"[{"epoch":97,"hash":"fc1ea09623d42ffb8270d439da2f6c33265e75e3e0c35caae924110bc5298b3e","certificate_hash":"efbaae947e817eb4c84b50335ace6d9141182efa1f3279f1c0c298748b55caed","created_at":"2023-09-29T00:03:14.022008952Z"},{"epoch":96,"hash":"620b62ec3c3071f621705c956914474c82bcd1735ade1b383b9b7679d8ee63f9","certificate_hash":"a4ae6264cb14dbcce1a71f7651b53a9dd77a96699d6ed39711d1ebfcc3096c49","created_at":"2023-09-24T00:03:30.174359552Z"},{"epoch":95,"hash":"d2ef323b45112e1c3aa596230650fc2281b9bf87efcab56f08d0d9829f4ed025","certificate_hash":"2992ff59eed5f60e170979d1b69d34e314413980cf377321efa7b8919bcb7365","created_at":"2023-09-19T00:04:10.715418520Z"},{"epoch":94,"hash":"0be6a9719ea55bd12c46a2ff9f224afc420e6c0f1f55a29dbfc19832adb4e99b","certificate_hash":"f41804e187a6ff0f26931705323f4582e7bd131b4e97f2513bc69317f36c225e","created_at":"2023-09-14T00:03:17.466444132Z"}]"#
}

pub struct AppState {
    epoch_settings: String,
    certificates: BTreeMap<String, Value>,
    snapshots: BTreeMap<String, Value>,
    msds: BTreeMap<String, Value>,
}

pub type SharedState = Arc<RwLock<AppState>>;

impl From<AppState> for SharedState {
    fn from(value: AppState) -> Self {
        Arc::new(RwLock::new(value))
    }
}

impl Default for AppState {
    fn default() -> Self {
        let epoch_settings = epoch_settings().to_string();
        let certificates: BTreeMap<String, Value> = serde_json::from_str::<Value>(certificates())
            .unwrap()
            .as_array()
            .expect("Given JSON String must be an array of certificates.")
            .into_iter()
            .map(|obj| {
                (
                    obj["hash"]
                        .as_str()
                        .expect("certificates must have 'hash' field.")
                        .to_owned(),
                    obj.to_owned(),
                )
            })
            .collect();
        let snapshots: BTreeMap<String, Value> = serde_json::from_str::<Value>(snapshots())
            .unwrap()
            .as_array()
            .expect("Given JSON string must be an array of snapshots.")
            .into_iter()
            .map(|obj| {
                (
                    obj["digest"]
                        .as_str()
                        .expect("snapshots must have 'digest' field.")
                        .to_owned(),
                    obj.to_owned(),
                )
            })
            .collect();
        let msds: BTreeMap<String, Value> = serde_json::from_str::<Value>(msds())
            .unwrap()
            .as_array()
            .expect("Given JSON string must be an array of Mithril Stake Distributions.")
            .into_iter()
            .map(|obj| {
                (
                    obj["hash"]
                        .as_str()
                        .expect("Mithril Stake Distributions must have a 'party_id' field.")
                        .to_owned(),
                    obj.to_owned(),
                )
            })
            .collect();

        Self {
            epoch_settings,
            certificates,
            snapshots,
            msds,
        }
    }
}

impl AppState {
    pub async fn get_snapshots(&self) -> StdResult<String> {
        let values: Vec<Value> = self.snapshots.iter().map(|(_k, v)| v.to_owned()).collect();

        serde_json::to_string(&Value::Array(values))
            .map_err(|e| anyhow!(e))
            .with_context(|| "could not JSON serialize the snapshots list.")
    }

    pub async fn get_msds(&self) -> StdResult<String> {
        let values: Vec<Value> = self.msds.iter().map(|(_k, v)| v.to_owned()).collect();

        serde_json::to_string(&Value::Array(values))
            .map_err(|e| anyhow!(e))
            .with_context(|| "could not JSON serialize the mithril stake distributions list.")
    }

    pub async fn get_snapshot(&self, key: &str) -> StdResult<Option<String>> {
        self.snapshots
            .get(key)
            .map(|v| serde_json::to_string(v))
            .transpose()
            .map_err(|e| e.into())
    }

    pub async fn get_msd(&self, key: &str) -> StdResult<Option<String>> {
        self.msds
            .get(key)
            .map(|v| serde_json::to_string(v))
            .transpose()
            .map_err(|e| e.into())
    }

    pub async fn get_epoch_settings(&self) -> StdResult<String> {
        Ok(self.epoch_settings.to_owned())
    }

    pub async fn get_certificates(&self) -> StdResult<String> {
        let values: Vec<Value> = self
            .certificates
            .iter()
            .map(|(_k, v)| v.to_owned())
            .collect();

        serde_json::to_string(&Value::Array(values))
            .map_err(|e| anyhow!(e))
            .with_context(|| "could not JSON serialize the certificates list.")
    }

    pub async fn get_certificate(&self, key: &str) -> StdResult<Option<String>> {
        self.certificates
            .get(key)
            .map(|v| serde_json::to_string(v))
            .transpose()
            .map_err(|e| e.into())
    }
}
/*
/// Return the content of a file in a directory if it exists.
/// If the file does not exist, it returns None.
fn open_file(dirpath: &StdPath, filename: &str) -> StdResult<Option<String>> {
    if !dirpath.exists() {
        return Err(anyhow!(format!(
            "Path '{}' does not exist.",
            dirpath.display()
        )));
    }

    if !dirpath.is_dir() {
        return Err(anyhow!(format!(
            "Path '{}' is not a directory!",
            dirpath.display()
        )));
    }
    let filepath = dirpath.to_owned().join(filename);

    if !filepath.exists() {
        return Ok(None);
    }

    if !filepath.is_file() {
        return Err(anyhow!(format!(
            "Path '{}' is not a data file.",
            filepath.display()
        )));
    }

    std::fs::read_to_string(filepath)
        .map(|s| Some(s))
        .map_err(|e| e.into())
}
*/
