use semver::Version;

pub const VERSION_10_6_2: Version = Version::new(10, 6, 2);

pub fn is_version_equal_or_upper(version_to_check: &str, version_reference: Version) -> bool {
    let normalized_version = version_to_check.trim().to_ascii_lowercase();
    if normalized_version == "latest" {
        return true;
    }

    match Version::parse(&normalized_version) {
        Ok(v) => v >= version_reference,
        Err(_) => false,
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compare_version() {
        assert!(!is_version_equal_or_upper(
            "not a string number",
            VERSION_10_6_2
        ));
        assert!(!is_version_equal_or_upper("0", VERSION_10_6_2));
        assert!(!is_version_equal_or_upper("10.5.1", VERSION_10_6_2));

        assert!(is_version_equal_or_upper("10.6.2", VERSION_10_6_2));
        assert!(is_version_equal_or_upper("10.7.3", VERSION_10_6_2));
        assert!(is_version_equal_or_upper("latest", VERSION_10_6_2));
    }
}
