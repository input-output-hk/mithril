use std::{
    fs::File,
    io::{BufReader, Read},
    path::PathBuf,
};

use anyhow::{Context, anyhow};
use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
use sha2::{Digest, Sha256};

use crate::StmResult;

/// TODO: remove allow(dead_code) when the constants are used or remove the constatnts
#[allow(dead_code)]
/// Constant storing the hash of the SRS of degree 22 used to create proof in production
///
/// If the degree of the SRS used were to change, this hash would need to be updated using
/// the proper value available here: https://github.com/midnightntwrk/midnight-trusted-setup/blob/main/MIDNIGHT_SRS_CATALOG.md
const SRS_HASH_K22: &str = "e8ad5eed936d657a0fb59d2a55ba19f81a3083bb3554ef88f464f5377e9b2c2f";
#[allow(dead_code)]
/// Constant storing temporary path of the SRS of degree 22 used to create proof in production
const SRS_PATH_K22: &str = "/tmp/trusted_setup/midnight-srs-2p22";
#[allow(dead_code)]
/// Constant storing URL to download the SRS of degree 22 used to create proof in production
const SRS_URL_K22: &str = "https://srs.midnight.network/midnight-srs-2p22";

/// Manages the local storage and integrity verification of an SRS file.
/// Stores the path of the local file, its expected hash value and a
/// URL for where to download it if the file is missing locally.
pub struct SrsManager {
    path: PathBuf,
    expected_hash: String,
    url: String,
}

/// TODO: remove allow(dead_code) when used
#[allow(dead_code)]
impl SrsManager {
    fn new(
        path: impl Into<PathBuf>,
        expected_hash: impl Into<String>,
        url: impl Into<String>,
    ) -> Self {
        Self {
            path: path.into(),
            expected_hash: expected_hash.into(),
            url: url.into(),
        }
    }

    /// Computes the SHA256 hash of the given bytes and returns its hex encoding.
    fn compute_hash(bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);

        hex::encode(hasher.finalize())
    }

    /// Reads the local SRS file at the stored path and checks its SHA256 hash against the expected value.
    // This function can be removed if it is not used.
    fn verify_hash(&self) -> StmResult<bool> {
        println!(
            "Verifying integrity of the download file by checking its Sha256 hash value. Expected hash: {:?}",
            self.expected_hash
        );
        let mut file = File::open(&self.path)
            .with_context(|| "Loading of the SRS file should have succeeded!")?;
        let mut srs_buffer = vec![];
        file.read_to_end(&mut srs_buffer)
            .with_context(|| "Reading the SRS file should have succeeded!")?;

        Ok(self.verify_bytes(&srs_buffer))
    }

    /// Checks SHA256 hash of the given bytes against the stored expected value.
    fn verify_bytes(&self, srs_bytes: &[u8]) -> bool {
        let recomputed_hash = Self::compute_hash(srs_bytes);
        println!("Hash of the given bytes: {:?}", recomputed_hash);

        self.expected_hash == recomputed_hash
    }

    /// Fetches the SRS from `self.url` and returns its bytes.
    fn download(&self) -> StmResult<Vec<u8>> {
        let response = reqwest::blocking::Client::builder()
            // TODO: For now a timeout but this should be updated depending on the behavior we want
            .timeout(std::time::Duration::from_secs(600))
            .build()?
            .get(&self.url)
            .header("User-Agent", "mithril-stm")
            .send()?
            .error_for_status()?;
        let bytes = response.bytes()?;

        Ok(bytes.to_vec())
    }

    /// Saves the given bytes in a file at the stored path while creating
    /// the directories of the path if needed.
    fn store(&self, srs_bytes: &[u8]) -> StmResult<()> {
        let parent = self
            .path
            .parent()
            .ok_or(anyhow!("The given path contains no parent directory!"))?;
        println!(
            "Creating temporary directories to store SRS file: {:?}",
            parent
        );
        std::fs::create_dir_all(parent)
            .with_context(|| "Subdirectory creation should have succeeded!")?;

        std::fs::write(&self.path, srs_bytes)?;
        Ok(())
    }

    /// Ensures the SRS file is present. If the file is missing,
    /// downloads it, verifies its hash and stores it if the hash is valid.
    fn ensure_available(&self) -> StmResult<()> {
        if !self.path.exists() {
            println!("File missing for local storage. Downloading and storing it locally.");
            println!(
                "Download SRS at URL: {:?} and storing it in file: {:?}",
                self.url, self.path
            );
            let srs_bytes = self
                .download()
                .with_context(|| "Download of the SRS file should have succeeded!")?;
            println!(
                "Verifying integrity of the given bytes by checking its Sha256 hash value. Expected hash: {:?}",
                self.expected_hash
            );
            if !self.verify_bytes(&srs_bytes) {
                return Err(anyhow!(
                    "Error, the hash of the SRS file does not match the hard-coded value!"
                ));
            }
            println!(
                "Integrity check passed, the SRS file was correctly downloaded. Saving it to disk..."
            );
            self.store(&srs_bytes)
                .with_context(|| "Saving the SRS to disk should have succeeded!")?;
            println!("The SRS file was correctly saved to disk and can be used securely.");
        }

        Ok(())
    }

    /// Ensures the SRS file is available, downloading it if necessary
    /// and deserializes it into memory.
    fn load(&self) -> StmResult<ParamsKZG<Bls12>> {
        self.ensure_available()?;

        let file = File::open(&self.path)
            .with_context(|| format!("Failed to open SRS file at {:?}", self.path))?;
        let mut reader = BufReader::new(file);

        ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytes)
            .with_context(|| "Failed to deserialize SRS from file")
    }
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;
    use tempfile::NamedTempFile;

    use super::*;

    const SRS_HASH_K1: &str = "bbe04fe3c70d0c138447cb086b4baddc30cb8bb2a004114bc02e6f739516280e";

    // Can be moved to a file in an asset directory
    const SRS_K1: &[u8; 772] = &[
        1, 0, 0, 0, 23, 241, 211, 167, 49, 151, 215, 148, 38, 149, 99, 140, 79, 169, 172, 15, 195,
        104, 140, 79, 151, 116, 185, 5, 161, 78, 58, 63, 23, 27, 172, 88, 108, 85, 232, 63, 249,
        122, 26, 239, 251, 58, 240, 10, 219, 34, 198, 187, 8, 179, 244, 129, 227, 170, 160, 241,
        160, 158, 48, 237, 116, 29, 138, 228, 252, 245, 224, 149, 213, 208, 10, 246, 0, 219, 24,
        203, 44, 4, 179, 237, 208, 60, 199, 68, 162, 136, 138, 228, 12, 170, 35, 41, 70, 197, 231,
        225, 21, 176, 173, 99, 244, 249, 255, 16, 204, 15, 172, 98, 0, 72, 248, 188, 99, 134, 184,
        91, 165, 82, 104, 25, 167, 240, 44, 241, 22, 110, 22, 153, 65, 156, 8, 179, 10, 142, 78,
        128, 243, 209, 17, 13, 50, 163, 45, 245, 16, 124, 133, 229, 90, 163, 193, 6, 96, 106, 49,
        225, 51, 203, 31, 64, 83, 232, 27, 240, 224, 46, 118, 112, 208, 26, 51, 6, 200, 126, 61,
        238, 129, 167, 217, 107, 169, 15, 82, 187, 244, 42, 187, 171, 185, 103, 67, 72, 8, 38, 246,
        126, 89, 155, 211, 217, 41, 203, 21, 58, 68, 77, 94, 162, 224, 172, 97, 77, 138, 1, 50, 81,
        76, 12, 42, 139, 177, 226, 14, 80, 158, 177, 21, 144, 203, 217, 32, 181, 188, 166, 81, 188,
        230, 151, 135, 171, 20, 243, 44, 204, 170, 114, 100, 20, 255, 137, 169, 91, 55, 231, 255,
        10, 137, 141, 197, 138, 133, 211, 195, 7, 206, 34, 63, 178, 0, 167, 170, 174, 55, 172, 160,
        66, 2, 103, 113, 188, 85, 98, 73, 144, 236, 129, 50, 191, 8, 250, 143, 167, 57, 187, 53, 6,
        192, 78, 148, 54, 52, 60, 187, 221, 248, 176, 163, 14, 245, 135, 74, 190, 149, 65, 43, 252,
        26, 173, 64, 18, 59, 177, 21, 118, 236, 165, 44, 177, 155, 65, 243, 49, 140, 215, 245, 105,
        13, 63, 226, 237, 85, 23, 33, 99, 233, 109, 45, 72, 207, 211, 52, 69, 121, 77, 156, 236,
        164, 52, 110, 29, 200, 76, 71, 187, 55, 202, 112, 172, 172, 51, 125, 240, 41, 10, 48, 12,
        252, 217, 29, 214, 149, 243, 242, 88, 19, 224, 43, 96, 82, 113, 159, 96, 125, 172, 211,
        160, 136, 39, 79, 101, 89, 107, 208, 208, 153, 32, 182, 26, 181, 218, 97, 187, 220, 127,
        80, 73, 51, 76, 241, 18, 19, 148, 93, 87, 229, 172, 125, 5, 93, 4, 43, 126, 2, 74, 162,
        178, 240, 143, 10, 145, 38, 8, 5, 39, 45, 197, 16, 81, 198, 228, 122, 212, 250, 64, 59, 2,
        180, 81, 11, 100, 122, 227, 209, 119, 11, 172, 3, 38, 168, 5, 187, 239, 212, 128, 86, 200,
        193, 33, 189, 184, 6, 6, 196, 160, 46, 167, 52, 204, 50, 172, 210, 176, 43, 194, 139, 153,
        203, 62, 40, 126, 133, 167, 99, 175, 38, 116, 146, 171, 87, 46, 153, 171, 63, 55, 13, 39,
        92, 236, 29, 161, 170, 169, 7, 95, 240, 95, 121, 190, 12, 229, 213, 39, 114, 125, 110, 17,
        140, 201, 205, 198, 218, 46, 53, 26, 173, 253, 155, 170, 140, 189, 211, 167, 109, 66, 154,
        105, 81, 96, 209, 44, 146, 58, 201, 204, 59, 172, 162, 137, 225, 147, 84, 134, 8, 184, 40,
        1, 4, 187, 225, 162, 79, 204, 79, 152, 140, 110, 242, 104, 208, 193, 22, 14, 172, 10, 12,
        79, 83, 216, 11, 215, 79, 61, 46, 70, 103, 190, 39, 64, 134, 37, 168, 56, 37, 53, 78, 39,
        199, 8, 89, 136, 49, 2, 235, 67, 7, 172, 181, 105, 179, 24, 124, 15, 209, 153, 57, 128,
        170, 82, 166, 233, 226, 8, 11, 150, 151, 250, 185, 106, 189, 92, 95, 28, 59, 152, 130, 86,
        242, 217, 147, 102, 241, 187, 204, 241, 60, 240, 226, 7, 2, 254, 225, 140, 15, 8, 23, 150,
        4, 171, 232, 193, 130, 11, 190, 209, 17, 39, 64, 141, 203, 80, 114, 173, 202, 184, 87, 116,
        163, 45, 81, 139, 104, 35, 80, 176, 106, 34, 168, 123, 241, 120, 135, 115, 42, 10, 244, 93,
        223, 204, 191, 248, 16, 225, 178, 33, 226, 165, 145, 29, 111, 150, 131, 163, 111, 78, 127,
        231, 212, 66, 129, 222, 134, 161, 134, 204, 16, 108, 51, 54, 245, 143, 236, 224, 30, 118,
        109, 196, 20, 125, 56, 227, 25, 54, 16, 90, 73, 68, 203, 89,
    ];

    #[test]
    fn valid_file_hash_succeeds() {
        let srs_file = NamedTempFile::new_in("/tmp").unwrap();
        std::fs::write(&srs_file, SRS_K1).unwrap();

        let result = SrsManager::new(srs_file.path(), SRS_HASH_K1, "")
            .verify_hash()
            .unwrap();

        assert!(result);
    }

    #[test]
    fn invalid_file_hash_fails() {
        let mut tampered_bytes = SRS_K1.to_vec();
        tampered_bytes[0] = tampered_bytes[0].wrapping_add(1);

        let result = SrsManager::new("", SRS_HASH_K1, "").verify_bytes(&tampered_bytes);

        assert!(!result);
    }

    #[test]
    fn valid_hash_bytes_succeeds() {
        let result = SrsManager::new("", SRS_HASH_K1, "").verify_bytes(SRS_K1);

        assert!(result);
    }

    #[test]
    fn invalid_hash_bytes_fails() {
        let srs_file = NamedTempFile::new_in("/tmp").unwrap();
        let mut tampered = SRS_K1.to_vec();
        tampered[0] = tampered[0].wrapping_add(1);
        std::fs::write(&srs_file, &tampered).unwrap();

        let result = SrsManager::new(srs_file.path(), SRS_HASH_K1, "")
            .verify_hash()
            .unwrap();

        assert!(!result);
    }

    #[test]
    fn existing_file_on_disk_skips_download() {
        let srs_file = NamedTempFile::new_in("/tmp").unwrap();
        std::fs::write(&srs_file, [0, 1, 2, 3, 4]).unwrap();

        let result = SrsManager::new(srs_file.path(), "", "").ensure_available();

        assert!(result.is_ok());
    }

    mod mock_server_test {
        use super::*;

        #[test]
        fn missing_srs_file_triggers_download() {
            let server = MockServer::start();
            let mock = server.mock(|when, then| {
                when.method(httpmock::Method::GET).path("/srs");
                then.status(200).body(SRS_K1);
            });

            let temp_dir = tempfile::tempdir().unwrap();
            let srs_path = temp_dir.path().join("missing_file_trigger_dl");

            SrsManager::new(&srs_path, SRS_HASH_K1, server.url("/srs"))
                .ensure_available()
                .unwrap();

            mock.assert();
            assert!(srs_path.exists());
        }

        #[test]
        fn downloaded_file_with_wrong_hash_fails_and_does_not_store_file() {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(httpmock::Method::GET).path("/srs");
                then.status(200).body(b"tampered content");
            });

            let temp_dir = tempfile::tempdir().unwrap();
            let srs_path = temp_dir.path().join("dl_wrong_hash");

            let result =
                SrsManager::new(&srs_path, SRS_HASH_K1, server.url("/srs")).ensure_available();

            assert!(result.is_err());
            assert!(!srs_path.exists());
        }

        #[test]
        fn server_error_fails() {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(httpmock::Method::GET).path("/srs");
                then.status(404);
            });

            let temp_dir = tempfile::tempdir().unwrap();
            let srs_path = temp_dir.path().join("server_error_fails");

            let result =
                SrsManager::new(&srs_path, SRS_HASH_K1, server.url("/srs")).ensure_available();

            assert!(result.is_err());
        }
    }
}
