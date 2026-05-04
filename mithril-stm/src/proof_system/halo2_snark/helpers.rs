use std::{fs::File, io::Read, path::Path};

use anyhow::{Context, anyhow};
use sha2::{Digest, Sha256};

use crate::StmResult;

const SRS_HASH_K22: &str = "e8ad5eed936d657a0fb59d2a55ba19f81a3083bb3554ef88f464f5377e9b2c2f";
const SRS_PATH_K22: &str = "/tmp/trusted_setup/midnight-srs-2p22";
const SRS_URL_K22: &str = "https://srs.midnight.network/midnight-srs-2p22";

fn verify_srs_file_hash(srs_path: &Path, srs_hash: &str) -> StmResult<bool> {
    println!(
        "Verifying integrity of the download file by checking its Sha256 hash value. Expected hash: {:?}",
        srs_hash
    );
    let mut file =
        File::open(srs_path).with_context(|| "Loading of the SRS file should have succeeded!")?;
    let mut srs_buffer = vec![];
    file.read_to_end(&mut srs_buffer)
        .with_context(|| "Reading the SRS file should have succeeded!")?;

    let mut hasher = Sha256::new();
    hasher.update(srs_buffer);

    let recomputed_srs_hash = hex::encode(hasher.finalize());
    println!("Hash of the local SRS file: {:?}", recomputed_srs_hash);

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

    if !srs_file_path.exists() {
        println!("File missing for local storage. Downloading and storing in temporary directory.");
        let parent = srs_file_path.parent().ok_or(anyhow!(
            "Parent directory for the given file does not exists!"
        ))?;
        println!(
            "Creating temporary directories to store SRS file: {:?}",
            parent
        );
        std::fs::create_dir_all(parent)
            .with_context(|| "Subdirectory creation should have succeeded!")?;
        println!(
            "Download SRS at URL: {:?} and storing it in file: {:?}",
            srs_url, srs_file_path
        );
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
                ));
            }
        }
        println!(
            "Integrity check passed, the SRS file was correctly downloaded and can be used securely."
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;
    use tempfile::NamedTempFile;

    use super::*;

    const SRS_HASH_K1: &str = "bbe04fe3c70d0c138447cb086b4baddc30cb8bb2a004114bc02e6f739516280e";

    // Can be move to a file in an asset directory
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
        let dummy_srs_path = NamedTempFile::new_in("/tmp").unwrap();
        std::fs::write(&dummy_srs_path, &SRS_K1).unwrap();

        let result = verify_srs_file_hash(dummy_srs_path.path(), SRS_HASH_K1).unwrap();

        assert_eq!(result, true);
    }

    #[test]
    fn invalid_file_hash_fails() {
        // Creates a dummy file to store the tampered SRS
        let dummy_srs_path = NamedTempFile::new_in("/tmp").unwrap();
        let mut srs_1_buff = SRS_K1.to_vec();
        // Tampers the first byte of SRS_K1
        srs_1_buff[0] += 1;
        std::fs::write(&dummy_srs_path, &srs_1_buff).unwrap();

        let result = verify_srs_file_hash(dummy_srs_path.path(), SRS_HASH_K1).unwrap();

        assert_eq!(result, false);
    }

    #[test]
    fn any_file_on_disk_succeeds() {
        let dummy_srs_path = NamedTempFile::new_in("/tmp").unwrap();
        // Writes some bytes in the dummy file
        let bytes = vec![0, 1, 2, 3, 4];
        std::fs::write(&dummy_srs_path, &bytes).unwrap();

        let result = check_and_verify_stored_srs(dummy_srs_path.path().to_str().unwrap(), "", "");

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
            let dummy_path = temp_dir.path().join("missing_file_trigger_dl");

            check_and_verify_stored_srs(
                dummy_path.to_str().unwrap(),
                SRS_HASH_K1,
                &server.url("/srs"),
            )
            .unwrap();

            mock.assert();
        }

        #[test]
        fn downloaded_file_with_wrong_hash_fails() {
            let server = MockServer::start();
            server.mock(|when, then| {
                when.method(httpmock::Method::GET).path("/srs");
                then.status(200).body(b"tampered content");
            });

            let temp_dir = tempfile::tempdir().unwrap();
            let srs_path = temp_dir.path().join("dl_wrong_hash");

            let result = check_and_verify_stored_srs(
                srs_path.to_str().unwrap(),
                SRS_HASH_K1,
                &server.url("/srs"),
            );

            assert!(result.is_err());
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

            let result = check_and_verify_stored_srs(
                srs_path.to_str().unwrap(),
                SRS_HASH_K1,
                &server.url("/srs"),
            );

            assert!(result.is_err());
        }
    }
}
