#[cfg(feature = "future_snark")]
use mithril_stm::{
    MERKLE_TREE_DEPTH_FOR_SNARK, Parameters,
    circuits::halo2::compute_prodution_non_recursive_circuit_verification_key,
};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let srs_path = args.get(1).expect("Provide a correct path for the SRS file.");

    let params = Parameters {
        m: 16948,
        k: 1944,
        phi_f: 0.2,
    };
    let merkle_tree_depth = MERKLE_TREE_DEPTH_FOR_SNARK;

    let verification_key = compute_prodution_non_recursive_circuit_verification_key(
        &params,
        merkle_tree_depth,
        srs_path,
    );

    println!("{:?}", verification_key);
}
