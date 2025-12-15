use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Write as _;
use std::fs;
use std::fs::File;
use std::path::Path;

use serde_json;

pub type ArtifactId = String;
pub type FileContent = String;

/// In memory representation of a folder containing data imported using the `scripts/import.sh` script
/// of the fake aggregator.
#[derive(Debug, Default)]
pub struct FakeAggregatorData {
    status: FileContent,

    epoch_settings: FileContent,

    certificates_list: FileContent,
    individual_certificates: BTreeMap<ArtifactId, FileContent>,

    snapshots_list: FileContent,
    individual_snapshots: BTreeMap<ArtifactId, FileContent>,

    mithril_stake_distributions_list: FileContent,
    individual_mithril_stake_distributions: BTreeMap<ArtifactId, FileContent>,

    cardano_transaction_snapshots_list: FileContent,
    individual_cardano_transaction_snapshots: BTreeMap<ArtifactId, FileContent>,
    cardano_transaction_proofs: BTreeMap<ArtifactId, FileContent>,

    cardano_stake_distributions_list: FileContent,
    individual_cardano_stake_distributions: BTreeMap<ArtifactId, FileContent>,

    cardano_database_snapshots_list: FileContent,
    individual_cardano_database_snapshots: BTreeMap<ArtifactId, FileContent>,
}

impl FakeAggregatorData {
    pub fn load_from_folder(folder: &Path) -> Self {
        let mut data = FakeAggregatorData::default();

        for entry in list_json_files_in_folder(folder) {
            let filename = entry.file_name().to_string_lossy().to_string();
            let file_content = fs::read_to_string(entry.path()).unwrap_or_else(|_| {
                panic!(
                    "Could not read file content, file_path: {}",
                    entry.path().display()
                )
            });

            match filename.as_str() {
                "status.json" => {
                    data.status = file_content;
                }
                "epoch-settings.json" => {
                    data.epoch_settings = file_content;
                }
                "mithril-stake-distributions-list.json" => {
                    data.mithril_stake_distributions_list = file_content;
                }
                "snapshots-list.json" => {
                    data.snapshots_list = file_content;
                }
                "cardano-stake-distributions-list.json" => {
                    data.cardano_stake_distributions_list = file_content;
                }
                "cardano-databases-list.json" => {
                    data.cardano_database_snapshots_list = file_content;
                }
                "certificates-list.json" => {
                    data.certificates_list = file_content;
                }
                "ctx-snapshots-list.json" => {
                    data.cardano_transaction_snapshots_list = file_content;
                }
                "mithril-stake-distributions.json" => {
                    data.individual_mithril_stake_distributions =
                        Self::read_artifacts_json_file(&entry.path());
                }
                "snapshots.json" => {
                    data.individual_snapshots = Self::read_artifacts_json_file(&entry.path());
                }
                "cardano-stake-distributions.json" => {
                    data.individual_cardano_stake_distributions =
                        Self::read_artifacts_json_file(&entry.path());
                }
                "cardano-databases.json" => {
                    data.individual_cardano_database_snapshots =
                        Self::read_artifacts_json_file(&entry.path());
                }
                "certificates.json" => {
                    data.individual_certificates = Self::read_artifacts_json_file(&entry.path());
                }
                "ctx-snapshots.json" => {
                    data.individual_cardano_transaction_snapshots =
                        Self::read_artifacts_json_file(&entry.path());
                }
                "ctx-proofs.json" => {
                    data.cardano_transaction_proofs = Self::read_artifacts_json_file(&entry.path());
                }
                // unknown file
                _ => {}
            }
        }

        data
    }

    pub fn generate_code_for_ids(self) -> String {
        let cardano_stake_distributions_per_epoch =
            extract_item_by_epoch(&self.individual_cardano_stake_distributions, "/epoch");
        let cardano_database_snapshots_per_epoch =
            extract_item_list_per_epoch(&self.cardano_database_snapshots_list, "/beacon/epoch");

        Self::assemble_code(
            &[
                generate_ids_array(
                    "snapshot_digests",
                    BTreeSet::from_iter(self.individual_snapshots.keys().cloned()),
                ),
                generate_ids_array(
                    "mithril_stake_distribution_hashes",
                    BTreeSet::from_iter(
                        self.individual_mithril_stake_distributions.keys().cloned(),
                    ),
                ),
                generate_ids_array(
                    "cardano_stake_distribution_hashes",
                    BTreeSet::from_iter(
                        self.individual_cardano_stake_distributions.keys().cloned(),
                    ),
                ),
                generate_epoch_array(
                    "cardano_stake_distribution_epochs",
                    BTreeSet::from_iter(cardano_stake_distributions_per_epoch.keys().cloned()),
                ),
                generate_ids_array(
                    "cardano_database_snapshot_hashes",
                    BTreeSet::from_iter(self.individual_cardano_database_snapshots.keys().cloned()),
                ),
                generate_epoch_array(
                    "cardano_database_snapshot_epochs",
                    BTreeSet::from_iter(cardano_database_snapshots_per_epoch.keys().cloned()),
                ),
                generate_ids_array(
                    "certificate_hashes",
                    BTreeSet::from_iter(self.individual_certificates.keys().cloned()),
                ),
                generate_ids_array(
                    "cardano_transaction_snapshot_hashes",
                    BTreeSet::from_iter(
                        self.individual_cardano_transaction_snapshots.keys().cloned(),
                    ),
                ),
                generate_ids_array(
                    "proof_transaction_hashes",
                    BTreeSet::from_iter(self.cardano_transaction_proofs.keys().cloned()),
                ),
            ],
            false,
        )
    }

    pub fn generate_code_for_all_data(self) -> String {
        let cardano_stake_distributions_per_epoch =
            extract_item_by_epoch(&self.individual_cardano_stake_distributions, "/epoch");
        let cardano_database_snapshots_per_epoch =
            extract_item_list_per_epoch(&self.cardano_database_snapshots_list, "/beacon/epoch");

        Self::assemble_code(
            &[
                generate_list_getter("status", self.status),
                generate_list_getter("epoch_settings", self.epoch_settings),
                generate_ids_array(
                    "snapshot_digests",
                    BTreeSet::from_iter(self.individual_snapshots.keys().cloned()),
                ),
                generate_artifact_getter("snapshots", self.individual_snapshots),
                generate_list_getter("snapshot_list", self.snapshots_list),
                generate_ids_array(
                    "mithril_stake_distribution_hashes",
                    BTreeSet::from_iter(
                        self.individual_mithril_stake_distributions.keys().cloned(),
                    ),
                ),
                generate_artifact_getter(
                    "mithril_stake_distributions",
                    self.individual_mithril_stake_distributions,
                ),
                generate_list_getter(
                    "mithril_stake_distribution_list",
                    self.mithril_stake_distributions_list,
                ),
                generate_ids_array(
                    "cardano_stake_distribution_hashes",
                    BTreeSet::from_iter(
                        self.individual_cardano_stake_distributions.keys().cloned(),
                    ),
                ),
                generate_epoch_array(
                    "cardano_stake_distribution_epochs",
                    BTreeSet::from_iter(cardano_stake_distributions_per_epoch.keys().cloned()),
                ),
                generate_artifact_per_epoch_getter(
                    "cardano_stake_distributions_per_epoch",
                    extract_item_by_epoch(&self.individual_cardano_stake_distributions, "/epoch"),
                ),
                generate_artifact_getter(
                    "cardano_stake_distributions",
                    self.individual_cardano_stake_distributions,
                ),
                generate_list_getter(
                    "cardano_stake_distribution_list",
                    self.cardano_stake_distributions_list,
                ),
                generate_ids_array(
                    "certificate_hashes",
                    BTreeSet::from_iter(self.individual_certificates.keys().cloned()),
                ),
                generate_ids_array(
                    "cardano_database_snapshot_hashes",
                    BTreeSet::from_iter(self.individual_cardano_database_snapshots.keys().cloned()),
                ),
                generate_epoch_array(
                    "cardano_database_snapshot_epochs",
                    BTreeSet::from_iter(cardano_database_snapshots_per_epoch.keys().cloned()),
                ),
                generate_artifact_getter(
                    "cardano_database_snapshots",
                    self.individual_cardano_database_snapshots,
                ),
                generate_list_getter(
                    "cardano_database_snapshot_list",
                    self.cardano_database_snapshots_list,
                ),
                generate_artifact_per_epoch_getter(
                    "cardano_database_snapshot_list_per_epoch",
                    cardano_database_snapshots_per_epoch,
                ),
                generate_artifact_getter("certificates", self.individual_certificates),
                generate_list_getter("certificate_list", self.certificates_list),
                generate_ids_array(
                    "cardano_transaction_snapshot_hashes",
                    BTreeSet::from_iter(
                        self.individual_cardano_transaction_snapshots.keys().cloned(),
                    ),
                ),
                generate_artifact_getter(
                    "cardano_transaction_snapshots",
                    self.individual_cardano_transaction_snapshots,
                ),
                generate_list_getter(
                    "cardano_transaction_snapshots_list",
                    self.cardano_transaction_snapshots_list,
                ),
                generate_ids_array(
                    "proof_transaction_hashes",
                    BTreeSet::from_iter(self.cardano_transaction_proofs.keys().cloned()),
                ),
                generate_artifact_getter(
                    "cardano_transaction_proofs",
                    self.cardano_transaction_proofs,
                ),
            ],
            true,
        )
    }

    fn assemble_code(functions_code: &[String], include_use_btree_map: bool) -> String {
        format!(
            "{}{}
",
            if include_use_btree_map {
                "use std::collections::BTreeMap;

"
            } else {
                ""
            },
            functions_code.join(
                "

"
            )
        )
    }

    fn read_artifacts_json_file(json_file: &Path) -> BTreeMap<ArtifactId, FileContent> {
        let file = File::open(json_file).unwrap();
        let parsed_json: serde_json::Value = serde_json::from_reader(&file).unwrap();

        let json_object = parsed_json.as_object().unwrap();
        let res: Result<Vec<_>, _> = json_object
            .iter()
            .map(|(key, value)| extract_artifact_id_and_content(key, value))
            .collect();

        BTreeMap::from_iter(res.unwrap())
    }
}

fn extract_artifact_id_and_content(
    key: &String,
    value: &serde_json::Value,
) -> Result<(ArtifactId, FileContent), String> {
    let json_content = serde_json::to_string_pretty(value).map_err(|e| e.to_string())?;
    Ok((key.to_owned(), json_content))
}

/// Takes a map of json string indexed by hashes and re-indexes them using their epoch
///
/// Each item in the map must contain an epoch value at the specified JSON pointer location.
pub fn extract_item_by_epoch(
    items_per_hash: &BTreeMap<String, String>,
    json_pointer_for_epoch: &str,
) -> BTreeMap<u64, String> {
    let mut res = BTreeMap::new();

    for (key, value) in items_per_hash {
        let parsed_json: serde_json::Value = serde_json::from_str(value)
            .unwrap_or_else(|_| panic!("Could not parse JSON entity '{key}'"));
        let epoch = parsed_json
            .pointer(json_pointer_for_epoch)
            .unwrap_or_else(|| panic!("missing `{json_pointer_for_epoch}` for JSON entity '{key}'"))
            .as_u64()
            .unwrap_or_else(|| {
                panic!("`{json_pointer_for_epoch}` is not a number for JSON entity '{key}'")
            });
        res.insert(epoch, value.clone());
    }

    res
}

/// Takes a JSON string containing a list of items and extracts them into a map keyed by epoch.
///
/// Each item in the list must contain an epoch value at the specified JSON pointer location.
pub fn extract_item_list_per_epoch(
    source: &str,
    json_pointer_for_epoch: &str,
) -> BTreeMap<u64, String> {
    let parsed_json: Vec<serde_json::Value> =
        serde_json::from_str(source).expect("Failed to parse JSON list");
    let mut list_per_epoch = BTreeMap::<u64, Vec<serde_json::Value>>::new();

    for item in parsed_json {
        let epoch = item
            .pointer(json_pointer_for_epoch)
            .unwrap_or_else(|| panic!("missing `{json_pointer_for_epoch}` for a json value"))
            .as_u64()
            .unwrap_or_else(|| panic!("`{json_pointer_for_epoch}` is not a number"));
        list_per_epoch.entry(epoch).or_default().push(item);
    }

    list_per_epoch
        .into_iter()
        .map(|(k, v)| (k, serde_json::to_string(&v).unwrap()))
        .collect()
}

pub fn list_json_files_in_folder(folder: &Path) -> impl Iterator<Item = fs::DirEntry> + '_ {
    crate::list_files_in_folder(folder)
        .filter(|e| e.file_name().to_string_lossy().ends_with(".json"))
}

// pub(crate) fn $fun_name()() -> BTreeMap<String, String>
pub fn generate_artifact_getter(
    fun_name: &str,
    source_jsons: BTreeMap<ArtifactId, FileContent>,
) -> String {
    let mut artifacts_list = String::new();

    for (artifact_id, file_content) in source_jsons {
        write!(
            artifacts_list,
            r###"
        (
            "{artifact_id}",
            r#"{file_content}"#
        ),"###
        )
        .unwrap();
    }

    format!(
        r###"pub(crate) fn {fun_name}() -> BTreeMap<String, String> {{
    [{artifacts_list}
    ]
    .into_iter()
    .map(|(k, v)| (k.to_owned(), v.to_owned()))
    .collect()
}}"###
    )
}

// pub(crate) fn $fun_name()() -> BTreeMap<u64, String>
pub fn generate_artifact_per_epoch_getter(
    fun_name: &str,
    source_jsons: BTreeMap<u64, FileContent>,
) -> String {
    let mut artifacts_list = String::new();

    for (artifact_id, file_content) in source_jsons {
        write!(
            artifacts_list,
            r###"
        (
            {artifact_id},
            r#"{file_content}"#
        ),"###
        )
        .unwrap();
    }

    format!(
        r###"pub(crate) fn {fun_name}() -> BTreeMap<u64, String> {{
    [{artifacts_list}
    ]
    .into_iter()
    .map(|(k, v)| (k.to_owned(), v.to_owned()))
    .collect()
}}"###
    )
}

/// pub(crate) fn $fun_name() -> &'static str
pub fn generate_list_getter(fun_name: &str, source_json: FileContent) -> String {
    format!(
        r###"pub(crate) fn {fun_name}() -> &'static str {{
    r#"{source_json}"#
}}"###
    )
}

/// pub(crate) fn $array_name() -> [&'a str; $ids.len]
pub fn generate_ids_array(array_name: &str, ids: BTreeSet<ArtifactId>) -> String {
    let mut ids_list = String::new();

    for id in &ids {
        write!(
            ids_list,
            r#"
        "{id}","#
        )
        .unwrap();
    }

    format!(
        r###"pub(crate) const fn {}<'a>() -> [&'a str; {}] {{
    [{}
    ]
}}"###,
        array_name,
        ids.len(),
        ids_list,
    )
}

/// pub(crate) fn $array_name() -> [u64; $epoch.len]
pub fn generate_epoch_array(array_name: &str, epoch: BTreeSet<u64>) -> String {
    let mut ids_list = String::new();

    for id in &epoch {
        write!(
            ids_list,
            r#"
        {id},"#
        )
        .unwrap();
    }

    format!(
        r###"pub(crate) const fn {}() -> [u64; {}] {{
    [{}
    ]
}}"###,
        array_name,
        epoch.len(),
        ids_list,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::get_temp_dir;

    #[test]
    fn generate_ids_array_with_empty_data() {
        assert_eq!(
            generate_ids_array("snapshots_digests", BTreeSet::new()),
            "pub(crate) const fn snapshots_digests<'a>() -> [&'a str; 0] {
    [
    ]
}"
        );
    }

    #[test]
    fn generate_ids_array_with_non_empty_data() {
        assert_eq!(
            generate_ids_array(
                "snapshots_digests",
                BTreeSet::from_iter(["abc".to_string(), "def".to_string(), "hij".to_string()])
            ),
            r#"pub(crate) const fn snapshots_digests<'a>() -> [&'a str; 3] {
    [
        "abc",
        "def",
        "hij",
    ]
}"#
        );
    }

    #[test]
    fn assemble_code_with_btree_use() {
        assert_eq!(
            "use std::collections::BTreeMap;

fn a() {}

fn b() {}
",
            FakeAggregatorData::assemble_code(
                &["fn a() {}".to_string(), "fn b() {}".to_string()],
                true
            )
        )
    }

    #[test]
    fn assemble_code_without_btree_use() {
        assert_eq!(
            "fn a() {}

fn b() {}
",
            FakeAggregatorData::assemble_code(
                &["fn a() {}".to_string(), "fn b() {}".to_string()],
                false
            )
        )
    }

    #[test]
    fn parse_artifacts_json_into_btree_of_key_and_pretty_sub_json() {
        let dir = get_temp_dir("read_artifacts_json_file");
        let file = dir.join("test.json");
        fs::write(
            &file,
            r#"{
    "hash1": { "name": "artifact1" },
    "hash2": { "name": "artifact2" }
}"#,
        )
        .unwrap();

        let id_per_json = FakeAggregatorData::read_artifacts_json_file(&file);

        let expected = BTreeMap::from([
            (
                "hash1".to_string(),
                r#"{
  "name": "artifact1"
}"#
                .to_string(),
            ),
            (
                "hash2".to_string(),
                r#"{
  "name": "artifact2"
}"#
                .to_string(),
            ),
        ]);
        assert_eq!(expected, id_per_json);
    }

    #[test]
    fn test_extract_item_by_epoch_by_epoch_with_valid_data() {
        let items_per_hash = BTreeMap::from([
            (
                "hash1".to_string(),
                r#"{"bar":4,"epoch":3,"foo":"...","hash":"2"}"#.to_string(),
            ),
            (
                "hash2".to_string(),
                r#"{"bar":7,"epoch":2,"foo":"...","hash":"1"}"#.to_string(),
            ),
        ]);

        // note: values are not re-serialized, so they are kept as is
        let item_per_epoch = extract_item_by_epoch(&items_per_hash, "/epoch");
        assert_eq!(
            BTreeMap::from([
                (3, items_per_hash.get("hash1").unwrap().to_string()),
                (2, items_per_hash.get("hash2").unwrap().to_string())
            ]),
            item_per_epoch
        )
    }

    #[test]
    #[should_panic(expected = "Could not parse JSON entity 'csd-123'")]
    fn test_extract_item_by_epoch_by_epoch_with_invalid_json() {
        let mut items_per_hash = BTreeMap::new();
        items_per_hash.insert(
            "csd-123".to_string(),
            r#""hash": "csd-123", "epoch": "123"#.to_string(),
        );

        extract_item_by_epoch(&items_per_hash, "/epoch");
    }

    #[test]
    #[should_panic(expected = "missing `/epoch` for JSON entity 'csd-123'")]
    fn test_extract_item_by_epoch_with_missing_epoch() {
        let mut items_per_hash = BTreeMap::new();
        items_per_hash.insert("csd-123".to_string(), r#"{"hash": "csd-123"}"#.to_string());

        extract_item_by_epoch(&items_per_hash, "/epoch");
    }

    #[test]
    fn test_extract_item_by_epoch_with_empty_map() {
        let items_per_hash = BTreeMap::new();

        let epochs = extract_item_by_epoch(&items_per_hash, "/epoch");

        assert!(epochs.is_empty());
    }

    #[test]
    fn test_extract_item_list_per_epoch_for_epoch() {
        let list_per_epoch_json = r#"[
                { "beacon": { "epoch": 1, "bar": 4 }, "hash":"3","foo":"..." },
                { "beacon": { "epoch": 2}, "hash":"2","foo":"..." },
                { "beacon": { "epoch": 1}, "hash":"1","foo":"..." }
            ]"#;

        // note: values are re-serialized, so serde_json reorders the keys
        let map_per_epoch = extract_item_list_per_epoch(list_per_epoch_json, "/beacon/epoch");
        assert_eq!(
            BTreeMap::from([
                (
                    1,
                    r#"[{"beacon":{"bar":4,"epoch":1},"foo":"...","hash":"3"},{"beacon":{"epoch":1},"foo":"...","hash":"1"}]"#
                        .to_string()
                ),
                (2, r#"[{"beacon":{"epoch":2},"foo":"...","hash":"2"}]"#.to_string()),
            ]),
            map_per_epoch
        )
    }

    #[test]
    #[should_panic(expected = "Failed to parse JSON list")]
    fn test_extract_item_list_per_epoch_with_invalid_json() {
        // invalid because of the trailing comma
        let list_per_epoch_json =
            r#"[ { "beacon": { "epoch": 1, "bar": 4 }, "hash":"3","foo":"..." }, ]"#;

        extract_item_list_per_epoch(list_per_epoch_json, "/epoch");
    }

    #[test]
    #[should_panic(expected = "missing `/epoch` for a json value")]
    fn test_extract_item_list_per_epoch_with_missing_epoch() {
        let list_per_epoch_json = r#"[ { "beacon": { "bar": 4 }, "hash":"3","foo":"..." } ]"#;

        extract_item_list_per_epoch(list_per_epoch_json, "/epoch");
    }

    #[test]
    fn test_extract_item_list_per_epoch_with_list() {
        let list_per_epoch_json = "[]";

        let epochs = extract_item_list_per_epoch(list_per_epoch_json, "/epoch");

        assert!(epochs.is_empty());
    }
}
