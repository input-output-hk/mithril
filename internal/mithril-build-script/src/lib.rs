use std::path::PathBuf;

pub mod fake_aggregator;

pub fn get_package_path(package_name: &str) -> PathBuf {
    let cargo_pkgid_output = std::process::Command::new(env!("CARGO"))
        .args(["pkgid", "--quiet", "-p", package_name])
        .output()
        .unwrap();

    match cargo_pkgid_output.status.success() {
        true => {
            let package_name = std::str::from_utf8(&cargo_pkgid_output.stdout)
                .unwrap()
                .trim()
                .strip_prefix("path+file://")
                .unwrap()
                .split('#')
                .collect::<Vec<_>>()[0];

            PathBuf::from(package_name)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_package_path_should_return_path_of_existing_package() {
        let expected = PathBuf::from("./../mithril-aggregator/")
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
}
