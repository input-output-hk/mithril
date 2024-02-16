// build.rs

use mithril_build_script::fake_aggregator::DataFolder;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("imported_data.rs");

    let data_folder_path: &Path = Path::new("./default_data");
    let data_folder = DataFolder::load_from_folder(data_folder_path);
    let generated_code = data_folder.generate_code_for_all_data();
    fs::write(dest_path, generated_code).unwrap();

    println!("cargo:rerun-if-changed=default_data/");
}
