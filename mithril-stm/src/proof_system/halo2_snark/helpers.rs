use std::{fs::File, io::Read, path::Path};

use anyhow::{Context, anyhow};
use sha2::{Digest, Sha256};

use crate::StmResult;

const SRS_HASH_K22: &str = "e8ad5eed936d657a0fb59d2a55ba19f81a3083bb3554ef88f464f5377e9b2c2f";
const SRS_PATH_K22: &str = "/tmp/trusted_setup/midnight-srs-2p22";
const SRS_URL_K22: &str = "https://srs.midnight.network/midnight-srs-2p22";

fn srs_file_exists(srs_path: &Path) -> bool {
    Path::new(srs_path).exists()
}

fn verify_srs_file_hash(srs_path: &Path, srs_hash: &str) -> StmResult<bool> {
    let srs_path = Path::new(srs_path);
    let mut file =
        File::open(srs_path).with_context(|| "Loading of the SRS file should have succeeded!")?;
    let mut srs_buffer = vec![];
    file.read_to_end(&mut srs_buffer)
        .with_context(|| "Reading the SRS file should have succeeded!")?;

    let mut hasher = Sha256::new();
    hasher.update(srs_buffer);

    let recomputed_srs_hash = hex::encode(hasher.finalize());

    Ok(srs_hash == recomputed_srs_hash)
}

fn download_srs_file(srs_path: &Path, srs_url: &str) -> StmResult<()> {
    // let response = reqwest::blocking::get(SRS_URL)?;
    let response = reqwest::blocking::Client::new()
        .get(srs_url)
        // TODO: Decide what to put in the value parameter of the reqwest
        .header("User-Agent", "mithril-stm")
        .send()?
        .error_for_status()?;
    let bytes = response.bytes()?;
    std::fs::write(srs_path, &bytes)?;

    Ok(())
}

/// A function that check
fn check_and_verify_stored_srs(srs_path: &str, srs_hash: &str, srs_url: &str) -> StmResult<()> {
    let srs_file_path = Path::new(srs_path);

    if !srs_file_exists(srs_file_path) {
        download_srs_file(srs_file_path, srs_url)?;
        let result_srs_check = verify_srs_file_hash(srs_file_path, srs_hash)?;
        match result_srs_check {
            true => {}
            false => {
                return Err(anyhow!(
                    "Error, the hash of the SRS file does not match the hard-coded value!"
                ))
                .into();
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SRS_HASH_K3: &str = "4be827a6472193df80d8f08b4b25a85baef436fdd1965d89b6af89f4ec4e99e2";
    const SRS_PATH_K3: &str = "/tmp/trusted_setup/midnight-srs-2p3";
    const SRS_URL_K3: &str = "https://srs.midnight.network/midnight-srs-2p3";

    #[test]
    fn test_quick_srs_hash() {
        let _ = check_and_verify_stored_srs(SRS_PATH_K3, SRS_HASH_K3, SRS_URL_K3);
    }

    #[test]
    fn test_dl_srs_file() {
        let _ = download_srs_file(Path::new(SRS_PATH_K3), SRS_URL_K3).unwrap();
    }

    #[test]
    fn missing_srs_file_triggers_download() {
        let srs_path = Path::new("/tmp/trusted_setup/midnight-srs-2p3");

        if srs_path.exists() {
            std::fs::remove_file(srs_path).unwrap();
        }
        check_and_verify_stored_srs(SRS_PATH_K3, SRS_HASH_K3, SRS_URL_K3).unwrap();

        assert!(srs_path.exists())
    }

    #[test]
    fn valid_file_hash_succeeds() {
        assert!(check_and_verify_stored_srs(SRS_PATH_K3, SRS_HASH_K3, SRS_URL_K3).is_ok());
    }

    #[test]
    fn invalid_file_hash_fails() {
        let mut invalid_srs_hash = String::from(SRS_HASH_K3);
        invalid_srs_hash.pop();
        // '5' is never the last digit of the midnight's hashes
        invalid_srs_hash.push('5');

        assert!(
            check_and_verify_stored_srs(SRS_PATH_K3, invalid_srs_hash.as_str(), SRS_URL_K3)
                .is_err()
        );
    }
}
