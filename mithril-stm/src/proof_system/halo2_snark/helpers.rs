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
        std::fs::create_dir_all(srs_file_path.parent().ok_or(anyhow!(
            "Parent directory for the given file does not exists!"
        ))?)
        .with_context(|| "Subdirectory creation should have succeeded!")?;
        download_srs_file(srs_file_path, srs_url)
            .with_context(|| "Download of the SRS file should have succeeded!")?;
        let result_srs_check = verify_srs_file_hash(srs_file_path, srs_hash).with_context(
            || "Verification of the hash of the downloaded file should have passed!",
        )?;
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
    use tempfile::NamedTempFile;

    use super::*;

    const SRS_HASH_K3: &str = "4be827a6472193df80d8f08b4b25a85baef436fdd1965d89b6af89f4ec4e99e2";
    const SRS_PATH_K3: &str = "/tmp/trusted_setup/midnight-srs-2p3";
    const SRS_URL_K3: &str = "https://srs.midnight.network/midnight-srs-2p3";

    #[test]
    fn missing_srs_file_triggers_download() {
        let dummy_path = "/tmp/trusted_setup/srs_for_test";
        let srs_path = Path::new(dummy_path);

        if srs_path.exists() {
            std::fs::remove_file(srs_path).unwrap();
        }
        check_and_verify_stored_srs(dummy_path, SRS_HASH_K3, SRS_URL_K3).unwrap();

        assert!(srs_path.exists());
        std::fs::remove_file(srs_path).unwrap();
    }

    #[test]
    fn valid_file_hash_succeeds() {
        let srs_path = Path::new(SRS_PATH_K3);

        let result = verify_srs_file_hash(srs_path, SRS_HASH_K3).unwrap();

        assert_eq!(result, true);
    }

    #[test]
    fn invalid_file_hash_fails() {
        let real_srs_path = Path::new(SRS_PATH_K3);
        // Creates a dummy file to store the tampered SRS
        let dummy_srs_path = NamedTempFile::new_in("/tmp").unwrap();
        // Creates and downloads the srs if it does not exists
        check_and_verify_stored_srs(SRS_PATH_K3, SRS_HASH_K3, SRS_URL_K3).unwrap();
        // Opens the srs file and loads its bytes
        let mut file = File::open(real_srs_path).unwrap();
        let mut srs_buffer = vec![];
        file.read_to_end(&mut srs_buffer).unwrap();
        // Tampers the first byte
        srs_buffer[0] += 1;
        // Saves the tampered SRS in the temporary file
        std::fs::write(&dummy_srs_path, &srs_buffer).unwrap();

        let result = verify_srs_file_hash(dummy_srs_path.path(), SRS_HASH_K3).unwrap();

        assert_eq!(result, false);
    }

    #[test]
    fn any_file_on_disk_succeeds() {
        let dummy_srs_path = NamedTempFile::new_in("/tmp").unwrap();

        if dummy_srs_path.path().exists() {
            let bytes = vec![0, 1, 2, 3, 4];
            std::fs::write(&dummy_srs_path, &bytes).unwrap();
        }

        assert!(
            check_and_verify_stored_srs(
                dummy_srs_path.path().to_str().unwrap(),
                SRS_HASH_K3,
                SRS_URL_K3
            )
            .is_ok()
        );
    }
}
