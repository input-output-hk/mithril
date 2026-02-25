use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter};

use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};

pub(crate) fn generate_params(k: u32, path: &str) -> ParamsKZG<Bls12> {
    let parent = std::path::Path::new(path).parent().unwrap();
    fs::create_dir_all(parent).unwrap();

    let params: ParamsKZG<Bls12> = ParamsKZG::unsafe_setup(k, ChaCha20Rng::seed_from_u64(42));

    let file = File::create(path).unwrap();
    let mut writer = BufWriter::new(file);

    params
        .write_custom(&mut writer, SerdeFormat::RawBytesUnchecked)
        .unwrap();

    params
}

pub(crate) fn load_params(path: &str) -> ParamsKZG<Bls12> {
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytesUnchecked).unwrap()
}
