use std::path::PathBuf;

pub mod fake_aggregator;

pub fn get_package_path(package_name: &str) -> PathBuf {
    let cargo_pkgid_output = std::process::Command::new(env!("CARGO"))
        .args(["pkgid", "--quiet", "-p", package_name])
        .output()
        .unwrap();

    match cargo_pkgid_output.status.success() {
        true => {
            let output_string = std::str::from_utf8(&cargo_pkgid_output.stdout)
                .unwrap()
                .trim();
            let package_path = extract_package_path(output_string);

            PathBuf::from(package_path)
        }
        false => {
            panic!(
                "cargo pkgid failed: stderr: {}",
                std::str::from_utf8(&cargo_pkgid_output.stderr)
                    .unwrap()
                    .trim()
            )
        }
    }
}

const PKGID_OUTPUT_PREFIX: &str = "file://";

fn extract_package_path<'a>(pkgid_output: &'a str) -> &'a str {
    let output_without_prefix = pkgid_output
        .split(PKGID_OUTPUT_PREFIX)
        .collect::<Vec<&'a str>>();

    output_without_prefix
        .last()
        .unwrap_or_else(|| {
            panic!("Could not remove '{PKGID_OUTPUT_PREFIX}' prefix from `cargo pkgid` output: {pkgid_output}")
        })
        .split('#')
        .collect::<Vec<_>>()
        .first()
        .unwrap_or_else(|| panic!("Could not remove '#x.y.z' suffix from `cargo pkgid` output: {pkgid_output}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_package_path_should_return_path_of_existing_package() {
        let expected = PathBuf::from("./../../mithril-aggregator/")
            .canonicalize()
            .unwrap();

        let package_path = get_package_path("mithril-aggregator");

        assert_eq!(package_path, expected);
    }

    #[test]
    #[should_panic]
    fn get_package_path_panic_if_valid_name_of_not_existing_package() {
        get_package_path("it-does-not-exist");
    }

    #[test]
    #[should_panic]
    fn get_package_path_panic_if_invalid_package_name() {
        get_package_path("Invalid Package Name ~~~");
    }

    #[test]
    fn extract_package_path_from_multiple_pkid_formats() {
        let expected = "/dev/package_path/crate";

        assert_eq!(
            extract_package_path(&format!("{PKGID_OUTPUT_PREFIX}{expected}#version")),
            expected
        );
        assert_eq!(
            extract_package_path(&format!("path+{PKGID_OUTPUT_PREFIX}{expected}#version")),
            expected
        );
    }
}
