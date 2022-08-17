use std::path::Path;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum DigestExtractError {
    #[error("Could not extract file stem from path: `{0}`")]
    FileStemExtractFailed(String),

    #[error("Could not found digest in the file name: `{0}`")]
    DigestNotFound(String),

    #[error("Invalid digest, all characters should be hexadecimal: `{0}`")]
    InvalidDigest(String),
}

/// Extract the digest contained in the given file path
///
/// The file path must be in the form `xxx.DIGEST.yyy`.
///
/// Please note that this is a crude implementation, we may want something stronger that can:
/// - takes other separators than just a dot.
/// - find the digest in any part of the file stem, not just only the one after the first dot.
/// - makes more check on the extracted digest.
pub fn extract_digest_from_path(filepath: &Path) -> Result<String, DigestExtractError> {
    let archive_name = filepath.file_stem().filter(|f| f.to_str().is_some());
    if archive_name.is_none() {
        return Err(DigestExtractError::FileStemExtractFailed(format!(
            "{}",
            filepath.display()
        )));
    }

    match archive_name.unwrap().to_str().unwrap().split('.').nth(1) {
        Some(digest) => {
            // @todo : maybe we should add a length check
            if digest.chars().all(|c| c.is_ascii_hexdigit()) {
                Ok(digest.to_string())
            } else {
                Err(DigestExtractError::InvalidDigest(digest.to_string()))
            }
        }
        None => Err(DigestExtractError::DigestNotFound(format!(
            "{}",
            filepath.display()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::extract_digest_from_path;
    use crate::tools::digest_helpers::DigestExtractError;
    use std::path::Path;

    #[test]
    fn should_extract_digest_from_path() {
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7".to_string();
        let filename = format!("testnet.{}.tar.gz", digest);
        let result = extract_digest_from_path(Path::new(&filename));

        assert_eq!(Ok(digest), result);
    }

    #[test]
    fn extract_digest_from_path_fail_if_file_named_incorrectly() {
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7".to_string();
        let filename = format!("{}.zip", digest);
        let result = extract_digest_from_path(Path::new(&filename));

        assert_eq!(Err(DigestExtractError::DigestNotFound(filename)), result);
    }

    #[test]
    fn extract_digest_from_path_fail_if_tar_file_named_incorrectly() {
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7".to_string();
        let filename = format!("{}.tar.gz", digest);
        let result = extract_digest_from_path(Path::new(&filename));

        assert_eq!(
            Err(DigestExtractError::InvalidDigest("tar".to_string())),
            result
        );
    }

    #[test]
    fn extract_digest_from_path_fail_if_digest_not_hexadecimal() {
        let digest = "not_hexadecimal".to_string();
        let filename = format!("testnet.{}.tar.gz", digest);
        let result = extract_digest_from_path(Path::new(&filename));

        assert_eq!(Err(DigestExtractError::InvalidDigest(digest)), result);
    }
}
