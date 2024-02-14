// build.rs

use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::Path;

type FileName = String;
type FileContent = String;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");
    let dest_path = Path::new("./tmp_default_values.rs");

    let data_folder_path: &Path = Path::new("./default_data");
    let data_folder = DataFolder::load_from_folder(data_folder_path);
    fs::write(dest_path, data_folder.generate_code()).unwrap();

    println!("cargo:rerun-if-changed=default_data/");
}

/// In memory representation of a folder containing data imported using the `scripts/import.sh` script.
///
/// List items are just their corresponding file content loaded in memory.
/// Individual items are btreemap with the source filename as key and the file content as value.
#[derive(Default)]
struct DataFolder {
    certificates_list: FileContent,
    individual_certificates: BTreeMap<FileName, FileContent>,

    snapshots_list: FileContent,
    individual_snapshots: BTreeMap<FileName, FileContent>,

    msds_list: FileContent,
    individual_msds: BTreeMap<FileName, FileContent>,

    ctx_commitments_list: FileContent,
    individual_ctx_commitments: BTreeMap<FileName, FileContent>,
}

impl DataFolder {
    fn load_from_folder(folder: &'static Path) -> Self {
        let mut data_folder = DataFolder::default();

        for entry in list_files_in_folder(folder) {
            let filename = entry.file_name().to_string_lossy().to_string();
            let file_content = fs::read_to_string(&entry.path()).unwrap_or_else(|_| {
                panic!(
                    "Could not read file content, file_path: {}",
                    entry.path().display()
                )
            });

            match filename.as_str() {
                "mithril-stake-distributions.json" => {
                    data_folder.msds_list = file_content;
                }
                "snapshots.json" => {
                    data_folder.snapshots_list = file_content;
                }
                "certificates.json" => {
                    data_folder.certificates_list = file_content;
                }
                "cardano-transactions.json" => {
                    data_folder.ctx_commitments_list = file_content;
                }
                _ if filename.starts_with("mithril-stake-distribution") => {
                    data_folder.individual_msds.insert(filename, file_content);
                }
                _ if filename.starts_with("snapshot") => {
                    data_folder
                        .individual_snapshots
                        .insert(filename, file_content);
                }
                _ if filename.starts_with("certificate") => {
                    data_folder
                        .individual_certificates
                        .insert(filename, file_content);
                }
                _ if filename.starts_with("cardano-transaction") => {
                    data_folder
                        .individual_ctx_commitments
                        .insert(filename, file_content);
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

{}

{}

{}
",
            generate_list_getter("snapshot_list", self.snapshots_list),
            generate_list_getter("msd_list", self.msds_list),
            generate_list_getter("certificate_list", self.certificates_list),
            generate_list_getter("ctx_commitments_list", self.ctx_commitments_list),
        )
    }
}

fn list_files_in_folder(folder: &'static Path) -> impl Iterator<Item = std::fs::DirEntry> {
    fs::read_dir(folder)
        .unwrap_or_else(|_| panic!("Could not read `{}` dir", folder.display()))
        .filter_map(|e| {
            let entry = e.unwrap_or_else(|_| {
                panic!("Failed to read a file in the `{}` dir", folder.display())
            });
            match entry.file_type() {
                Ok(file_type) if file_type.is_file() => Some(entry),
                _ => None,
            }
        })
}

/// pub fn $fun_name() -> BTreeMap<String, String>
fn generate_artifact_getter(
    fun_name: &str,
    source_jsons: BTreeMap<FileName, FileContent>,
) -> String {
    todo!()
    //     format!(
    //         r###"
    // pub(super) fn {}() -> BTreeMap<String, String> {{
    //     r#"{}"#
    // }}
    //     "###,
    //         fun_name, source_json
    //     )
}

/// pub fn $fun_name() -> &'static str
fn generate_list_getter(fun_name: &str, source_json: FileContent) -> String {
    format!(
        r###"pub(super) fn {}() -> &'static str {{
    r#"{}"#
}}"###,
        fun_name, source_json
    )
}
