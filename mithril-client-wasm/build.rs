// build.rs

use mithril_build_script::fake_aggregator::FakeAggregatorData;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("imported_data.rs");
    let fake_aggregator_crate_path =
        mithril_build_script::get_package_path("mithril-aggregator-fake");

    let data_folder_path = fake_aggregator_crate_path.join("default_data");
    let data = FakeAggregatorData::load_from_folder(&data_folder_path);
    let generated_code = data.generate_code_for_ids();
    fs::write(dest_path, generated_code).unwrap();

    println!(
        "cargo:rerun-if-changed={}/",
        fake_aggregator_crate_path.display()
    );
}
