#[cfg(feature = "future_snark")]
use mithril_stm::circuits::halo2_ivc::compute_prodution_recursive_circuit_verification_key;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let srs_path = args.get(1).expect("Provide a correct path for the SRS file.");

    let verification_key = compute_prodution_recursive_circuit_verification_key(srs_path);

    println!("{:?}", verification_key);
}
