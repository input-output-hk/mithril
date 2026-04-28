use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;
use std::sync::OnceLock;

use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};

static PARAMS_LOCK_DEGREE_13: OnceLock<()> = OnceLock::new();

fn write_srs_to_file(parent: &Path, path: &str, format: SerdeFormat, params: &ParamsKZG<Bls12>) {
    fs::create_dir_all(parent).expect("Failed to create the directories.");
    let file = File::create(path).expect("Failed to create the file.");
    let mut writer = BufWriter::new(file);
    params
        .write_custom(&mut writer, format)
        .expect("Failed to write the SRS in the file.");
}

pub(crate) fn generate_params(k: u32, path: &str, format: SerdeFormat) -> ParamsKZG<Bls12> {
    let parent = std::path::Path::new(path).parent().expect("No parent directory.");
    let params: ParamsKZG<Bls12> = ParamsKZG::unsafe_setup(k, ChaCha20Rng::seed_from_u64(42));
    PARAMS_LOCK_DEGREE_13.get_or_init(|| write_srs_to_file(parent, path, format, &params));

    params
}

pub(crate) fn load_params(path: &str, format: SerdeFormat) -> ParamsKZG<Bls12> {
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    ParamsKZG::read_custom(&mut reader, format)
        .expect("Failure while reading the SRS file during tests!")
}
