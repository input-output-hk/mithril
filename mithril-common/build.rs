// build.rs

use std::env;
use std::fs;
use std::path::Path;

use mithril_build_script::open_api::{
    generate_open_api_versions_mapping, list_all_open_api_spec_files,
};

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let open_api_spec_files = list_all_open_api_spec_files(&[Path::new("./"), (Path::new("../"))]);
    let open_api_code = generate_open_api_versions_mapping(&open_api_spec_files);

    fs::write(Path::new(&out_dir).join("open_api.rs"), open_api_code).unwrap();

    for open_api_spec_file in open_api_spec_files {
        println!(
            "cargo:rerun-if-changed={}",
            open_api_spec_file.to_string_lossy()
        );
    }
}
