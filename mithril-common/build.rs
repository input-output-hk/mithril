// build.rs

use mithril_build_script::open_api::generate_open_api_versions_mapping;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let open_api_code = generate_open_api_versions_mapping(&[Path::new("./"), (Path::new("../"))]);

    fs::write(Path::new(&out_dir).join("open_api.rs"), open_api_code).unwrap();
}
