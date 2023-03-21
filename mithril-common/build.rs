// build.rs

use glob::glob;
use semver::Version;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

type OpenAPIFileName = String;
type OpenAPIVersionRaw = String;

fn read_version_from_open_api_spec_file(spec_file_path: PathBuf) -> OpenAPIVersionRaw {
    let yaml_spec = std::fs::read_to_string(spec_file_path).unwrap();
    let open_api: serde_yaml::Value = serde_yaml::from_str(&yaml_spec).unwrap();
    open_api["info"]["version"].as_str().unwrap().to_owned()
}

fn list_all_open_api_spec_files() -> Vec<PathBuf> {
    let mut open_api_spec_files = Vec::new();
    for entry in glob("../openapi*.yaml").unwrap() {
        open_api_spec_files.push(entry.unwrap())
    }

    open_api_spec_files
}

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("open_api.rs");
    let open_api_spec_files = list_all_open_api_spec_files();
    let open_api_versions = open_api_spec_files
        .into_iter()
        .map(|path| (path.clone(), read_version_from_open_api_spec_file(path)))
        .map(|(path, version_raw)| {
            (
                path.file_name().unwrap().to_str().unwrap().to_string(),
                Version::parse(&version_raw).unwrap().to_string(),
            )
        })
        .collect::<HashMap<OpenAPIFileName, OpenAPIVersionRaw>>();
    let open_api_versions_json = format!(
        "r#\"{}\"#",
        serde_json::to_string(&open_api_versions).unwrap()
    );
    fs::write(
        dest_path,
        format!(
            r#"
/// Open API file name
pub type OpenAPIFileName = String;

/// Open PAI raw version
pub type OpenAPIVersionRaw = String;

/// Build Open API versions mapping
pub fn get_open_api_versions_mapping() -> HashMap<OpenAPIFileName, OpenAPIVersionRaw> {{
    serde_json::from_str({}).unwrap()
}}
        "#,
            open_api_versions_json
        ),
    )
    .unwrap();
}
