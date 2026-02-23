use semver::Version;

pub fn is_version_at_least_10_6_2_or_latest(version: &str) -> bool {
    let normalized_version = version.trim().to_ascii_lowercase();
    if normalized_version == "latest" {
        return true;
    }

    match Version::parse(&normalized_version) {
        Ok(v) => v >= Version::new(10, 6, 2),
        Err(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::is_version_at_least_10_6_2_or_latest;

    #[test]
    fn test_is_version_at_least_10_6_2_or_latest() {
        assert!(!is_version_at_least_10_6_2_or_latest("not a string number"));
        assert!(!is_version_at_least_10_6_2_or_latest("0"));
        assert!(!is_version_at_least_10_6_2_or_latest("10.5.1"));

        assert!(is_version_at_least_10_6_2_or_latest("10.6.2"));
        assert!(is_version_at_least_10_6_2_or_latest("10.7.3"));
        assert!(is_version_at_least_10_6_2_or_latest("latest"));
    }
}
