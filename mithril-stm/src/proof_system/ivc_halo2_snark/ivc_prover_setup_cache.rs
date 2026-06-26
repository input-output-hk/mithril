#[cfg(feature = "future_snark")]
use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, Mutex},
};

#[cfg(feature = "future_snark")]
use anyhow::anyhow;
#[cfg(feature = "future_snark")]
use midnight_zk_stdlib::MidnightVK;

#[cfg(feature = "future_snark")]
use crate::{
    Parameters, StmResult,
    circuits::{
        circuit_verification_key_provider::CircuitVerificationKeyProvider,
        trusted_setup::TrustedSetupProvider,
    },
    proof_system::ivc_halo2_snark::IvcSnarkProverSetup,
};

/// Process-wide cache of the IVC prover setup keyed by the certificate circuit shape.
///
/// The setup (SRS plus the certificate and IVC circuit keys) only depends on the protocol
/// parameters and the Merkle tree depth, so it is computed once per shape and reused across
/// certificates instead of being rebuilt on every proof. The certificate [MidnightVK] is cached
/// alongside it because it is needed to expose the verifier data.
#[cfg(feature = "future_snark")]
type IvcSnarkProverSetupCache = HashMap<(Vec<u8>, u32), (Arc<IvcSnarkProverSetup>, MidnightVK)>;

#[cfg(feature = "future_snark")]
static IVC_PROVER_SETUP_CACHE: LazyLock<Mutex<IvcSnarkProverSetupCache>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Load the IVC prover setup and the certificate [MidnightVK] for the given certificate circuit
/// shape, building them on the first call and serving cached clones afterwards.
///
/// The per-circuit keys are themselves disk-cached by the certificate and recursive
/// [`CircuitVerificationKeyProvider`]s; this cache memoizes the assembled, in-memory setup (and the
/// certificate [MidnightVK]) so the clerk reuses it across certificates instead of re-deriving the
/// fixed bases and re-reading the keys on every proof.
#[cfg(feature = "future_snark")]
pub(crate) fn load_ivc_prover_setup(
    parameters: Parameters,
    merkle_tree_depth: u32,
) -> StmResult<(Arc<IvcSnarkProverSetup>, MidnightVK)> {
    let cache_key = (parameters.to_bytes()?, merkle_tree_depth);

    if let Some(cached) = IVC_PROVER_SETUP_CACHE
        .lock()
        .map_err(|_| anyhow!("IVC prover setup cache lock poisoned."))?
        .get(&cache_key)
    {
        return Ok(cached.clone());
    }

    let trusted_setup_provider = TrustedSetupProvider::default();
    let certificate_provider =
        CircuitVerificationKeyProvider::for_non_recursive_circuit(&parameters, merkle_tree_depth)?;
    let ivc_setup = Arc::new(IvcSnarkProverSetup::load(
        &trusted_setup_provider,
        &certificate_provider,
        CircuitVerificationKeyProvider::for_recursive_circuit,
    )?);
    let certificate_midnight_verifying_key =
        ivc_setup.certificate_verifying_key.midnight_vk().clone();

    let cached = (ivc_setup, certificate_midnight_verifying_key);
    IVC_PROVER_SETUP_CACHE
        .lock()
        .map_err(|_| anyhow!("IVC prover setup cache lock poisoned."))?
        .insert(cache_key, cached.clone());

    Ok(cached)
}

#[cfg(test)]
mod tests {

    mod slow {
        use midnight_proofs::poly::commitment::Params;

        use crate::{
            Parameters,
            circuits::halo2_ivc::{
                RECURSIVE_CIRCUIT_DEGREE,
                tests::common::{
                    asset_readers::load_embedded_verification_context_asset,
                    generators::setup::{QUORUM_SIZE, SIGNER_COUNT},
                },
            },
            proof_system::ivc_halo2_snark::prover_setup::build_unsafe_ivc_setup,
        };

        // `build_unsafe_ivc_setup` loads from an oversized unsafe SRS that shares the production
        // SRS's tau, so the keys and stored SRS must both downsize to `RECURSIVE_CIRCUIT_DEGREE` to
        // reproduce the embedded production assets exactly.
        #[test]
        fn ivc_setup_downsizes_keys_and_srs_to_the_circuit_degree() {
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            let ivc_setup = build_unsafe_ivc_setup(parameters, merkle_tree_depth)
                .expect("IvcSnarkProverSetup::load should succeed");

            let verification_context = load_embedded_verification_context_asset()
                .expect("verification context asset should load");

            assert_eq!(
                verification_context.certificate_verifying_key.vk().transcript_repr(),
                ivc_setup
                    .certificate_verifying_key
                    .midnight_vk()
                    .vk()
                    .transcript_repr(),
                "cert VK must be independent of the SRS degree (downsized at keygen)"
            );
            assert_eq!(
                verification_context.recursive_verifying_key.transcript_repr(),
                ivc_setup.ivc_verifying_key.verifying_key().transcript_repr(),
                "IVC VK must be independent of the SRS degree (downsized at keygen)"
            );
            assert_eq!(
                ivc_setup.srs.max_k(),
                RECURSIVE_CIRCUIT_DEGREE,
                "the proving SRS stored in IvcSnarkProverSetup must be downsized to the IVC circuit degree"
            );
        }
    }
}
