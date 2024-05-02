use semver::Version;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

type OpenAPIFileName = String;
type OpenAPIVersionRaw = String;

const TYPE_ALIAS: &str = r"/// Open API file name
pub type OpenAPIFileName = String;
";

fn list_all_open_api_spec_files(paths: &[&Path]) -> Vec<PathBuf> {
    let mut open_api_spec_files = Vec::new();

    for path in paths {
        for entry in crate::list_files_in_folder(path).filter(|e| {
            let os_filename = e.file_name();
            let filename = os_filename.to_string_lossy();
            filename.starts_with("openapi") && filename.ends_with(".yaml")
        }) {
            open_api_spec_files.push(entry.path())
        }
    }

    open_api_spec_files
}

fn read_version_from_open_api_spec_file(spec_file_path: PathBuf) -> OpenAPIVersionRaw {
    let yaml_spec = fs::read_to_string(spec_file_path).unwrap();
    let open_api: serde_yaml::Value = serde_yaml::from_str(&yaml_spec).unwrap();
    open_api["info"]["version"].as_str().unwrap().to_owned()
}

/// Generate the `get_open_api_versions_mapping` function based on the Open API files
/// in the given folders.
pub fn generate_open_api_versions_mapping(paths: &[&Path]) -> String {
    let open_api_spec_files = list_all_open_api_spec_files(paths);
    // Use a BTreeMap to guarantee the deterministic code generation below
    let open_api_versions: BTreeMap<OpenAPIFileName, Version> = open_api_spec_files
        .into_iter()
        .map(|path| (path.clone(), read_version_from_open_api_spec_file(path)))
        .map(|(path, version_raw)| {
            (
                path.file_name().unwrap().to_string_lossy().to_string(),
                Version::parse(&version_raw).unwrap(),
            )
        })
        .collect();

    let mut open_api_versions_hashmap = String::new();
    for (filename, version) in open_api_versions {
        open_api_versions_hashmap.push_str(&format!(
            r#"("{filename}".to_string(), semver::Version::new({}, {}, {})), "#,
            version.major, version.minor, version.patch
        ));
    }

    format!(
        r#"{TYPE_ALIAS}
/// Build Open API versions mapping
pub fn get_open_api_versions_mapping() -> HashMap<OpenAPIFileName, semver::Version> {{
    HashMap::from([
        {}
    ])
}}
        "#,
        open_api_versions_hashmap
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::get_temp_dir;
    use std::path::Path;

    fn write_minimal_open_api_file(version: &str, path: &Path) {
        fs::write(
            path,
            format!(
                r#"openapi: "3.0.0"
info:
  version: {version}
  title: Minimal Open Api File
"#
            ),
        )
        .unwrap()
    }

    fn assert_open_api_content_contains(expected_content: &str, generated_code: &str) {
        assert!(
            generated_code.contains(expected_content),
            "generated code did not include expected openapi files entries:\
            \n---- Code that was expected to be included:\n{expected_content}\
            \n---- Actual generated code:{}",
            // Remove type aliases for readability
            generated_code.trim_start_matches(TYPE_ALIAS)
        );
    }

    #[test]
    fn generated_code_include_type_aliases() {
        let generated_code = generate_open_api_versions_mapping(&[Path::new("./")]);

        assert!(generated_code.contains(TYPE_ALIAS));
    }

    #[test]
    fn generated_function_returns_an_hashmap_of_open_api_file_name_and_semver_version() {
        let generated_code = generate_open_api_versions_mapping(&[Path::new("./")]);

        assert!(generated_code.contains("-> HashMap<OpenAPIFileName, semver::Version>"));
    }

    #[test]
    fn generate_code_from_a_simple_open_api_file() {
        let dir = get_temp_dir("generate_code_from_a_simple_open_api_file");
        write_minimal_open_api_file("1.0.0", &dir.join("openapi.yaml"));

        let expected = r#"("openapi.yaml".to_string(), semver::Version::new(1, 0, 0))"#;
        let generated_code = generate_open_api_versions_mapping(&[&dir]);

        assert_open_api_content_contains(expected, &generated_code);
    }

    #[test]
    fn only_read_yaml_files() {
        let dir = get_temp_dir("only_read_yaml_files");
        write_minimal_open_api_file("1.0.0", &dir.join("openapi.yaml"));
        fs::write(dir.join("openapi.json"), "{}").unwrap();

        let included_files = list_all_open_api_spec_files(&[&dir]);

        assert_eq!(vec![dir.join("openapi.yaml")], included_files);
    }

    #[test]
    fn generate_code_from_two_open_api_files_in_different_folders() {
        let sub_folder =
            get_temp_dir("generate_code_from_two_open_api_files_in_different_folders/subfolder");
        let parent_folder = sub_folder.parent().unwrap();
        write_minimal_open_api_file("1.0.0", &parent_folder.join("openapi.yaml"));
        write_minimal_open_api_file("2.0.0", &sub_folder.join("openapi-thales.yaml"));

        let expected = r#"("openapi-thales.yaml".to_string(), semver::Version::new(2, 0, 0)), ("openapi.yaml".to_string(), semver::Version::new(1, 0, 0))"#;
        let generated_code = generate_open_api_versions_mapping(&[parent_folder, &sub_folder]);

        assert_open_api_content_contains(expected, &generated_code);
    }

    #[test]
    fn when_colliding_filenames_version_is_read_from_latest_given_folder() {
        let sub_folder = get_temp_dir(
            "when_colliding_filenames_version_read_is_from_latest_given_folder/subfolder",
        );
        let parent_folder = sub_folder.parent().unwrap();
        write_minimal_open_api_file("1.0.0", &parent_folder.join("openapi.yaml"));
        write_minimal_open_api_file("2.0.0", &sub_folder.join("openapi.yaml"));

        let expected = r#"HashMap::from([
        ("openapi.yaml".to_string(), semver::Version::new(2, 0, 0)), 
    ])"#;
        let generated_code = generate_open_api_versions_mapping(&[parent_folder, &sub_folder]);

        assert_open_api_content_contains(expected, &generated_code);
    }
}
