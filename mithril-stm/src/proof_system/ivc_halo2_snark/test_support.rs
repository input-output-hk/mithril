//! Shared builders for the slow IVC prover-input tests.
//!
//! `build_ivc_setup` runs real keygen under the unsafe SRS; `wrap_snark_proof` and
//! `wrap_avk` reconstruct a certificate proof and an aggregate verification key from
//! stored asset bytes; `build_global` assembles the recursive `Global` from the
//! verification-context asset; `accumulator_bytes` serializes an accumulator for
//! byte-level comparison. Centralized here so the slow tests in `prover_input` and
//! `prover_input_helpers` do not duplicate them.

use std::sync::Arc;

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
use midnight_proofs::utils::SerdeFormat;
use tempfile::tempdir;

use crate::{
    AggregateVerificationKeyForSnark, MithrilMembershipDigest, Parameters, SnarkProof,
    circuits::{
        halo2_ivc::{
            K,
            io::Write as IvcWrite,
            state::Global,
            tests::common::{
                asset_readers::VerificationContextAsset,
                generators::{
                    build_recursive_global,
                    setup::{AssetGenerationSetup, QUORUM_SIZE, SIGNER_COUNT, TOTAL_STAKE},
                },
            },
        },
        trusted_setup::build_provider_with_unsafe_srs,
    },
    proof_system::halo2_snark::CircuitVerificationKey,
};

use super::{
    prover_setup::IvcProverSetup,
    unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider},
};

pub(crate) fn build_ivc_setup() -> IvcProverSetup {
    let temp_dir = tempdir().expect("temp dir creation should succeed");
    let trusted_setup_provider = build_provider_with_unsafe_srs(temp_dir.path(), K);
    let srs = Arc::new(
        trusted_setup_provider
            .get_trusted_setup_parameters()
            .expect("unsafe SRS should load"),
    );
    let parameters = Parameters {
        k: QUORUM_SIZE as u64,
        m: (QUORUM_SIZE * 10) as u64,
        phi_f: 0.2,
    };
    let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
    let cert_provider =
        TempCertificateKeyProvider::new(Arc::clone(&srs), parameters, merkle_tree_depth);
    let cert_vk = cert_provider
        .get_verifying_key()
        .expect("certificate verifying key keygen should succeed");
    let ivc_provider = TempIvcKeyProvider::new(srs, cert_vk);
    IvcProverSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
        .expect("IvcProverSetup::load should succeed under the unsafe SRS")
}

pub(crate) fn wrap_snark_proof(
    verification_context: &VerificationContextAsset,
    certificate_proof_bytes: Vec<u8>,
) -> SnarkProof<MithrilMembershipDigest> {
    let parameters = Parameters {
        k: QUORUM_SIZE as u64,
        m: (QUORUM_SIZE * 10) as u64,
        phi_f: 0.2,
    };
    let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
    let circuit_verification_key =
        CircuitVerificationKey::new(verification_context.certificate_verifying_key.clone());
    SnarkProof::from_parts(
        certificate_proof_bytes,
        parameters,
        merkle_tree_depth,
        circuit_verification_key,
    )
}

pub(crate) fn wrap_avk(
    aggregate_verification_key_merkle_root: &[u8; 32],
) -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
    let mut avk_bytes = [0u8; 40];
    avk_bytes[0..32].copy_from_slice(aggregate_verification_key_merkle_root);
    avk_bytes[32..40].copy_from_slice(&TOTAL_STAKE.to_be_bytes());
    AggregateVerificationKeyForSnark::<MithrilMembershipDigest>::from_bytes(&avk_bytes)
        .expect("AVK should decode from asset bytes")
}

pub(crate) fn build_global(
    asset_setup: &AssetGenerationSetup,
    verification_context: &VerificationContextAsset,
) -> Global {
    build_recursive_global(
        asset_setup,
        &verification_context.certificate_verifying_key,
        &verification_context.recursive_verifying_key,
    )
}

pub(crate) fn accumulator_bytes(accumulator: &Accumulator<BlstrsEmulation>) -> Vec<u8> {
    let mut bytes = Vec::new();
    accumulator
        .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
        .expect("accumulator serialization should succeed");
    bytes
}
