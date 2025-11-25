use anyhow::Context;
use std::path::Path;

use mithril_common::StdResult;

use crate::utils::get_process_path;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NodeVersion {
    semver_version: semver::Version,
}

impl NodeVersion {
    /// `NodeVersion` factory
    pub fn new(semver_version: semver::Version) -> Self {
        Self { semver_version }
    }

    /// Fetch the semver version of a binary
    ///
    /// The binary must have a `--version` command which returns the version in the following forms:
    /// `string x.y.z` where `x.y.z` is a semver version.
    pub fn fetch(bin_name: &str, bin_dir: &Path) -> StdResult<NodeVersion> {
        let process_path = get_process_path(bin_name, bin_dir)?;
        // Note: usage of blocking std::process::Command instead of tokio::process::Command to avoid making this method async
        // example output: mithril-client 0.12.33+3063c3e
        let output = std::process::Command::new(process_path)
            .args(["--version"])
            .output()
            .with_context(|| format!("failed to run `{bin_name} --version`"))?;

        if output.status.success() {
            let raw_output = String::from_utf8(output.stdout)
                .with_context(|| format!("failed to parse `{bin_name}` raw version to uft8"))?;
            let version_string = raw_output.split_whitespace().nth(1).with_context(|| {
                format!("could not find `{bin_name}` semver version; output: `{raw_output}`",)
            })?;

            semver::Version::parse(version_string)
                .with_context(|| {
                    format!(
                        "failed to parse `{bin_name}` semver version; input: `{version_string}`"
                    )
                })
                .map(NodeVersion::new)
        } else {
            let stdout = String::from_utf8(output.stdout).ok();
            let stderr = String::from_utf8(output.stderr).ok();
            anyhow::bail!(
                "`failed to fetch `{bin_name}` version; stdout: `{stdout:?}`; stderr: `{stderr:?}`"
            );
        }
    }

    /// Checks if the node version is strictly below the given version.
    ///
    /// Panics if `version` is not a valid semver version
    pub fn is_below(&self, version: &'static str) -> bool {
        let version_req = semver::VersionReq::parse(&format!("<{version}")).unwrap();
        version_req.matches(&self.semver_version)
    }

    /// Checks if the node version is equal or above the given version.
    ///
    /// Panics if `version` is not a valid semver version
    pub fn is_above_or_equal(&self, version: &'static str) -> bool {
        let version_req = semver::VersionReq::parse(&format!(">={version}")).unwrap();
        version_req.matches(&self.semver_version)
    }

    /// Checks if the node version is between the given min and max version
    ///
    /// Check against the min version is superior or equal, check against the max version is
    /// strictly below.
    ///
    /// Panics if either `min_version` or `max_version` are not a valid semver version
    pub fn is_between(&self, min_version: &'static str, max_version: &'static str) -> bool {
        let version_req =
            semver::VersionReq::parse(&format!(">={min_version}, <{max_version}")).unwrap();
        version_req.matches(&self.semver_version)
    }
}

impl From<&NodeVersion> for semver::Version {
    fn from(value: &NodeVersion) -> Self {
        value.semver_version.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_below() {
        let version = NodeVersion::new(semver::Version::new(1, 2, 3));

        assert!(version.is_below("1.2.4"));
        assert!(!version.is_below("1.2.3"));
        assert!(!version.is_below("1.2.2"));
    }

    #[test]
    fn test_version_equal_or_above() {
        let version = NodeVersion::new(semver::Version::new(5, 7, 1));

        assert!(version.is_above_or_equal("4.6.0"));
        assert!(version.is_above_or_equal("5.7.1"));
        assert!(!version.is_above_or_equal("5.7.2"));
    }

    #[test]
    fn test_version_between() {
        let version = NodeVersion::new(semver::Version::new(2, 4, 3));

        assert!(version.is_between("2.3.0", "2.5.0"));
        assert!(version.is_between("2.4.3", "2.5.0"));
        assert!(version.is_between("2.4.3", "2.4.4"));

        assert!(!version.is_between("2.3.0", "2.4.3"));
        assert!(!version.is_between("2.4.4", "2.5.0"));
    }

    // Unix only has those tests leverage shell scripts and unix permissions
    #[cfg(all(test, unix))]
    mod unix_only {
        use std::io::Write;
        use std::os::unix::fs::OpenOptionsExt;

        use mithril_common::temp_dir_create;

        use super::*;

        fn write_shell_script(file_name: &str, folder: &Path, content: &str) {
            let script_path = folder.join(file_name);
            let mut file = std::fs::OpenOptions::new()
                .create_new(true)
                .write(true)
                .mode(0o755)
                .open(&script_path)
                .unwrap();
            file.write_all(format!("#!/bin/bash\n\n{content}\n").as_ref())
                .unwrap();
        }

        #[test]
        fn fetch_version() {
            let temp_dir = temp_dir_create!();
            write_shell_script(
                "test-program",
                &temp_dir,
                r#"echo "test-program 1.24.109+2f7e87""#,
            );

            let version = NodeVersion::fetch("test-program", &temp_dir).unwrap();

            assert_eq!(
                NodeVersion::new(semver::Version::parse("1.24.109+2f7e87").unwrap()),
                version
            );
        }
    }
}
