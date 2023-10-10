use std::env;

fn main() {
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    let target_no_batch_verify_aggregates =
        target_arch.eq("wasm32") || env::var("STM_TEST_NO_BATCH_VERIFY_AGGREGATES").is_ok();

    if !target_no_batch_verify_aggregates {
        println!("cargo:rustc-cfg=feature=\"batch-verify-aggregates\"");
    }
}
