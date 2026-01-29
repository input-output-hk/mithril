use crate::circuits::halo2::golden::support::certificate_case::{
    run_certificate_case, run_certificate_case_default,
};
use crate::circuits::halo2::types::JubjubBase;
use ff::Field;

#[test]
fn test_certificate_baseline() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_certificate_case_default("small", K, QUORUM);
}

#[test]
fn test_certificate_medium() {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_certificate_case_default("medium", K, QUORUM);
}

#[test]
fn test_certificate_message_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_certificate_case("message_zero", K, QUORUM, JubjubBase::ZERO);
}

#[test]
fn test_certificate_message_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -JubjubBase::ONE; // p - 1
    run_certificate_case("message_max", K, QUORUM, msg_max);
}

// The following "large" test case is intentionally commented out.
// This test is extremely expensive (large K and quorum) and can take
// a very long time to run, which would make CI impractically heavy.
// In the future, we may introduce a dedicated benchmarking or ignored-test
// mechanism to re-enable it in a controlled way.
//
// #[test]
// fn test_certificate_large() {
//     const K: u32 = 21;
//     const QUORUM: u32 = 1024;
//     run_certificate_case_default("large", K, QUORUM);
// }
