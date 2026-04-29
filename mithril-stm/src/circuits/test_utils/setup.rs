use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter};

use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

pub(crate) fn generate_params(
    k: u32,
    path: &str,
    format: SerdeFormat,
    case_name: &str,
) -> ParamsKZG<Bls12> {
    let parent = std::path::Path::new(path).parent().expect("No parent directory.");
    let params: ParamsKZG<Bls12> = ParamsKZG::unsafe_setup(k, ChaCha20Rng::seed_from_u64(42));
    fs::create_dir_all(parent).expect("Failed to create the directories.");
    // Uses the name of the higher level test calling the function to create a temporary file
    // storing the srs and renames the file once it is done being written
    // The rename at the end is atomic on most config so it should be possible to access the file
    // during the renaming when several tests create the same file
    let tmp_path = format!("{}.tmp.{}", path, case_name);
    {
        let file = File::create(&tmp_path).expect("Failed to create the temp SRS file.");
        let mut writer = BufWriter::new(file);
        params
            .write_custom(&mut writer, format)
            .expect("Failed to write the SRS in the temp file.");
    }
    // Rename the temporary file holding the srs to the actual file
    fs::rename(&tmp_path, path).expect("Failed to atomically rename SRS temp file.");

    params
}

pub(crate) fn load_params(path: &str, format: SerdeFormat) -> ParamsKZG<Bls12> {
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    ParamsKZG::read_custom(&mut reader, format)
        .expect("Failure while reading the SRS file during tests!")
}
