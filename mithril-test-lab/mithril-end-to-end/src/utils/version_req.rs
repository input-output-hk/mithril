use anyhow::Context;
use std::path::Path;
use std::process::{Command, Output};

use mithril_common::StdResult;

use crate::utils::file_utils;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NodeVersion {
    semver_version: semver::Version,
}

impl NodeVersion {
    /// `NodeVersion` factory
    pub fn new(semver_version: semver::Version) -> Self {
        Self { semver_version }
    }

    /// Fetch the `NodeVersion` of a binary
    ///
    /// The binary must have a `--version` command which returns the version in the following forms:
    /// `string x.y.z` where `x.y.z` is a semver version.
    pub fn fetch(bin_name: &str, bin_dir: &Path) -> StdResult<NodeVersion> {
        Self::fetch_semver(bin_name, bin_dir).map(NodeVersion::new)
    }

    /// Fetch the semver version of a binary
    ///
    /// The binary must have a `--version` command which returns the version in the following forms:
    /// `string x.y.z` where `x.y.z` is a semver version.
    pub fn fetch_semver(bin_name: &str, bin_dir: &Path) -> StdResult<semver::Version> {
        let process_path = file_utils::get_process_path(bin_name, bin_dir)?;
        // Note: usage of blocking std::process::Command instead of tokio::process::Command to avoid making this method async
        let output = Command::new(&process_path)
            .args(["--version"])
            .output()
            .with_context(|| {
                format!(
                    "failed to run `{bin_name} --version`; bin_dir: `{}`; expanded process_path: `{}`",
                    bin_dir.display(),
                    process_path.display()
                )
            })?;

        parse_semver_version_from_process_output(bin_name, output)
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

/// Parse a semver version from a [Command::output]
///
/// Expect stdout to be in the form of: `mithril-client 0.12.33+3063c3e`
fn parse_semver_version_from_process_output(
    bin_name: &str,
    output: Output,
) -> StdResult<semver::Version> {
    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout).ok();
        let stderr = String::from_utf8(output.stderr).ok();
        anyhow::bail!(
            "`failed to fetch `{bin_name}` version; stdout: `{stdout:?}`; stderr: `{stderr:?}`"
        );
    }

    let raw_output = String::from_utf8(output.stdout)
        .with_context(|| format!("failed to parse `{bin_name}` raw version to uft8"))?;
    let version_string = raw_output.split_whitespace().nth(1).with_context(|| {
        format!("could not find `{bin_name}` semver version; output: `{raw_output}`",)
    })?;

    semver::Version::parse(version_string).with_context(|| {
        format!("failed to parse `{bin_name}` semver version; input: `{version_string}`")
    })
}

impl From<&NodeVersion> for semver::Version {
    fn from(value: &NodeVersion) -> Self {
        value.semver_version.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::process::ExitStatus;

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

    #[test]
    #[cfg(any(unix, windows))]
    fn test_version_output_parsing() {
        #[cfg(unix)]
        use std::os::unix::process::ExitStatusExt;
        #[cfg(windows)]
        use std::os::windows::process::ExitStatusExt;

        let output = Output {
            status: ExitStatus::from_raw(0),
            stdout: "test-program 1.24.109+2f7e87".as_bytes().to_vec(),
            stderr: vec![],
        };

        let version = parse_semver_version_from_process_output("test-program", output).unwrap();

        assert_eq!(semver::Version::parse("1.24.109+2f7e87").unwrap(), version);
    }
}
