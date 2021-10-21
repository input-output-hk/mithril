extern crate cbindgen;

use std::env;
use std::path::PathBuf;

/// Taken directly from cbindgen docs
fn main() {
    let include = PathBuf::from("target").join(PathBuf::from("include"));
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file(include.join("mithril.h"));
}
