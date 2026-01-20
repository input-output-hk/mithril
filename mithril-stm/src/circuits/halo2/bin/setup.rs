use rand_core::OsRng;
use std::env;
use std::fs;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

use midnight_curves::Bls12;
use midnight_proofs::{poly::kzg::params::ParamsKZG, utils::SerdeFormat};

fn create(k: u32, path: &str) {
    let parent = std::path::Path::new(path).parent().unwrap();
    fs::create_dir_all(parent).unwrap();

    let params: ParamsKZG<Bls12> = ParamsKZG::unsafe_setup(k, OsRng);

    let file = File::create(path).unwrap();
    let mut writer = BufWriter::new(file);

    params
        .write_custom(&mut writer, SerdeFormat::RawBytesUnchecked)
        .unwrap();

    println!("ParamsKZG written to {}", path);
}

fn open(path: &str) -> ParamsKZG<Bls12> {
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let params: ParamsKZG<Bls12> =
        ParamsKZG::read_custom(&mut reader, SerdeFormat::RawBytesUnchecked).unwrap();

    params
}

fn main() {
    // Usage: cargo run -p mithril-stm --features future_snark --bin setup -- <k>
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <k>", args[0]);
        std::process::exit(1);
    }

    let k: u32 = args[1].parse().expect("Invalid value for k, must be an integer");
    let manifest_dir =
        PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set"));
    let halo2_dir = manifest_dir.join("src").join("circuits").join("halo2");
    let path = halo2_dir.join("assets").join(format!("params_kzg_unsafe_{}", k));
    let path = path.to_string_lossy().to_string();

    create(k, &path);
    open(&path);
}
