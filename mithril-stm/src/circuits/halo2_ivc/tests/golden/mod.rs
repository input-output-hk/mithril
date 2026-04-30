//! Golden tests lock in recursive Halo2 IVC verification behavior.

pub(crate) const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;

mod positive;

#[cfg(test)]
mod golden_verification_key_test {

    use midnight_circuits::verifier::{BlstrsEmulation, SelfEmulation};
    use midnight_proofs::{
        plonk::{VerifyingKey, keygen_vk_with_k},
        poly::kzg::KZGCommitmentScheme,
        utils::SerdeFormat,
    };
    use midnight_zk_stdlib::MidnightCircuit;

    use crate::{
        Parameters,
        circuits::{
            halo2::circuit::StmCircuit,
            halo2_ivc::{
                circuit::IvcCircuit,
                tests::{
                    common::generators::setup::build_deterministic_params,
                    golden::RECURSIVE_CIRCUIT_DEGREE,
                },
            },
        },
    };

    // Golden test circuit verification key for the recursive circuit
    // This key is generated with an unsafe SRS generated on the fly
    // used to also generate a non-recursive verification key
    const GOLDEN_RECURSIVE_CIRCUIT_VERIFICATION_KEY: &[u8] =
        include_bytes!("../assets/golden_recursive_circuit_verification_key.bin");

    // Function used to compute the recursive circuit verification key for golden test
    // It uses an unsafe setup function to create the SRS
    fn golden_value_recursive_circuit_verification_key() -> Vec<u8> {
        let srs_for_recursive_circuit = build_deterministic_params(RECURSIVE_CIRCUIT_DEGREE);

        let small_parameters = Parameters {
            m: 10,
            k: 1,
            phi_f: 0.2,
        };
        let merkle_tree_depth = 3;
        let circuit = StmCircuit::try_new(&small_parameters, merkle_tree_depth).unwrap();
        let circuit_degree = MidnightCircuit::from_relation(&circuit).min_k();

        let mut srs_for_non_recursive_circuit = srs_for_recursive_circuit.clone();
        srs_for_non_recursive_circuit.downsize(circuit_degree);

        // Change the path to the settled path for the SRS
        let circuit_verification_key =
            midnight_zk_stdlib::setup_vk(&srs_for_non_recursive_circuit, &circuit);

        let default_ivc_circuit = IvcCircuit::unknown(circuit_verification_key.vk());
        let recursive_verifying_key: VerifyingKey<
            <BlstrsEmulation as SelfEmulation>::F,
            KZGCommitmentScheme<<BlstrsEmulation as SelfEmulation>::Engine>,
        > = keygen_vk_with_k(
            &srs_for_recursive_circuit,
            &default_ivc_circuit,
            RECURSIVE_CIRCUIT_DEGREE,
        )
        .expect("recursive verifying key generation should not fail");

        let mut buf_cvk = vec![];
        recursive_verifying_key
            .write(&mut buf_cvk, SerdeFormat::RawBytes)
            .unwrap();
        buf_cvk
    }

    mod slow {
        use super::*;

        #[test]
        pub fn golden_recursive_modification_test() {
            let golden_value = golden_value_recursive_circuit_verification_key();

            assert_eq!(
                GOLDEN_RECURSIVE_CIRCUIT_VERIFICATION_KEY,
                golden_value.as_slice()
            );
        }
    }
}
