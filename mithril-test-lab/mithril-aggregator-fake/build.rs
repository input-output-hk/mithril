// build.rs

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::env;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

type ArtifactId = String;
type FileContent = String;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("imported_data.rs");

    let data_folder_path: &Path = Path::new("./default_data");
    let data_folder = DataFolder::load_from_folder(data_folder_path);
    let generated_code = data_folder.generate_code();
    fs::write(dest_path, generated_code).unwrap();

    println!("cargo:rerun-if-changed=default_data/");
}

/// In memory representation of a folder containing data imported using the `scripts/import.sh` script.
///
/// List items are just their corresponding file content loaded in memory.
/// Individual items are btreemap with the source filename as key and the file content as value.
#[derive(Debug, Default)]
struct DataFolder {
    epoch_settings: FileContent,

    certificates_list: FileContent,
    individual_certificates: BTreeMap<ArtifactId, FileContent>,

    snapshots_list: FileContent,
    individual_snapshots: BTreeMap<ArtifactId, FileContent>,

    msds_list: FileContent,
    individual_msds: BTreeMap<ArtifactId, FileContent>,

    ctx_commitments_list: FileContent,
    individual_ctx_commitments: BTreeMap<ArtifactId, FileContent>,
    ctx_proofs: BTreeMap<ArtifactId, FileContent>,
}

impl DataFolder {
    fn load_from_folder(folder: &Path) -> Self {
        fn extract_artifact_id(filename: &str, prefix: &str) -> String {
            let id_with_extension = filename.strip_prefix(prefix).unwrap();
            id_with_extension.strip_suffix(".json").unwrap().to_string()
        }

        let mut data_folder = DataFolder::default();

        for entry in list_json_files_in_folder(folder) {
            let filename = entry.file_name().to_string_lossy().to_string();
            let file_content = fs::read_to_string(&entry.path()).unwrap_or_else(|_| {
                panic!(
                    "Could not read file content, file_path: {}",
                    entry.path().display()
                )
            });

            match filename.as_str() {
                "epoch-settings.json" => {
                    data_folder.epoch_settings = file_content;
                }
                "mithril-stake-distributions.json" => {
                    data_folder.msds_list = file_content;
                }
                "snapshots.json" => {
                    data_folder.snapshots_list = file_content;
                }
                "certificates.json" => {
                    data_folder.certificates_list = file_content;
                }
                "ctx-commitments.json" => {
                    data_folder.ctx_commitments_list = file_content;
                }
                _ if filename.starts_with("mithril-stake-distribution-") => {
                    data_folder.individual_msds.insert(
                        extract_artifact_id(&filename, "mithril-stake-distribution-"),
                        file_content,
                    );
                }
                _ if filename.starts_with("snapshot-") => {
                    data_folder
                        .individual_snapshots
                        .insert(extract_artifact_id(&filename, "snapshot-"), file_content);
                }
                _ if filename.starts_with("certificate-") => {
                    data_folder
                        .individual_certificates
                        .insert(extract_artifact_id(&filename, "certificate-"), file_content);
                }
                _ if filename.starts_with("ctx-commitment-") => {
                    data_folder.individual_ctx_commitments.insert(
                        extract_artifact_id(&filename, "ctx-commitment-"),
                        file_content,
                    );
                }
                _ if filename.starts_with("ctx-proof-") => {
                    data_folder
                        .ctx_proofs
                        .insert(extract_artifact_id(&filename, "ctx-proof-"), file_content);
                }
                // unknown file
                _ => {}
            }
        }

        data_folder
    }

    fn generate_code(self) -> String {
        format!(
            "use std::collections::BTreeMap;

{}
",
            [
                generate_list_getter("epoch_settings", self.epoch_settings),
                generate_ids_array(
                    "snapshot_digests",
                    BTreeSet::from_iter(self.individual_snapshots.keys().cloned())
                ),
                generate_artifact_getter("snapshots", self.individual_snapshots),
                generate_list_getter("snapshot_list", self.snapshots_list),
                generate_ids_array(
                    "msd_hashes",
                    BTreeSet::from_iter(self.individual_msds.keys().cloned())
                ),
                generate_artifact_getter("msds", self.individual_msds),
                generate_list_getter("msd_list", self.msds_list),
                generate_ids_array(
                    "certificate_hashes",
                    BTreeSet::from_iter(self.individual_certificates.keys().cloned())
                ),
                generate_artifact_getter("certificates", self.individual_certificates),
                generate_list_getter("certificate_list", self.certificates_list),
                generate_ids_array(
                    "ctx_commitment_hashes",
                    BTreeSet::from_iter(self.individual_ctx_commitments.keys().cloned())
                ),
                generate_artifact_getter("ctx_commitments", self.individual_ctx_commitments),
                generate_list_getter("ctx_commitments_list", self.ctx_commitments_list),
                generate_ids_array(
                    "proof_transaction_hashes",
                    BTreeSet::from_iter(self.ctx_proofs.keys().cloned())
                ),
                generate_artifact_getter("ctx_proofs", self.ctx_proofs),
            ]
            .join(
                "

"
            )
        )
    }
}

fn list_json_files_in_folder(folder: &Path) -> impl Iterator<Item = std::fs::DirEntry> + '_ {
    fs::read_dir(folder)
        .unwrap_or_else(|_| panic!("Could not read `{}` dir", folder.display()))
        .filter_map(move |e| {
            let entry = e.unwrap_or_else(|_| {
                panic!("Failed to read a file in the `{}` dir", folder.display())
            });
            match entry.file_type() {
                Ok(file_type) if file_type.is_file() => Some(entry),
                _ => None,
            }
        })
        .filter(|e| e.file_name().to_string_lossy().ends_with(".json"))
}

// pub(crate) fn $fun_name()() -> BTreeMap<String, String>
fn generate_artifact_getter(
    fun_name: &str,
    source_jsons: BTreeMap<ArtifactId, FileContent>,
) -> String {
    let mut artifacts_list = String::new();

    for (artifact_id, file_content) in source_jsons {
        write!(
            artifacts_list,
            r###"
        (
            "{}",
            r#"{}"#
        ),"###,
            artifact_id, file_content
        )
        .unwrap();
    }

    format!(
        r###"pub(crate) fn {}() -> BTreeMap<String, String> {{
    [{}
    ]
    .into_iter()
    .map(|(k, v)| (k.to_owned(), v.to_owned()))
    .collect()
}}"###,
        fun_name, artifacts_list
    )
}

/// pub(crate) fn $fun_name() -> &'static str
fn generate_list_getter(fun_name: &str, source_json: FileContent) -> String {
    format!(
        r###"pub(crate) fn {}() -> &'static str {{
    r#"{}"#
}}"###,
        fun_name, source_json
    )
}

/// pub(crate) fn $fun_name() -> &'static str
fn generate_ids_array(array_name: &str, ids: BTreeSet<ArtifactId>) -> String {
    let mut ids_list = String::new();

    for id in &ids {
        write!(
            ids_list,
            r#"
        "{}","#,
            id
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
