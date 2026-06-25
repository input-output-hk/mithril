// TODO: remove when the `IvcSnarkSetup` is final
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
    circuits::trusted_setup::TrustedSetupProvider,
    proof_system::ivc_halo2_snark::{
        IvcProverSetup, TempCertificateKeyProvider, TempIvcKeyProvider,
    },
};

/// Process-wide cache of the IVC prover setup keyed by the certificate circuit shape.
///
/// The setup (SRS plus the certificate and IVC circuit keys) only depends on the protocol
/// parameters and the Merkle tree depth, so it is computed once per shape and reused across
/// certificates instead of being rebuilt on every proof. The certificate [MidnightVK] is cached
/// alongside it because it is needed to expose the verifier data.
#[cfg(feature = "future_snark")]
type IvcProverSetupCache = HashMap<(Vec<u8>, u32), (Arc<IvcProverSetup>, MidnightVK)>;

#[cfg(feature = "future_snark")]
static IVC_PROVER_SETUP_CACHE: LazyLock<Mutex<IvcProverSetupCache>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Load the IVC prover setup and the certificate [MidnightVK] for the given certificate circuit
/// shape, building them on the first call and serving cached clones afterwards.
#[cfg(feature = "future_snark")]
pub(crate) fn load_ivc_prover_setup(
    parameters: Parameters,
    merkle_tree_depth: u32,
) -> StmResult<(Arc<IvcProverSetup>, MidnightVK)> {
    let cache_key = (parameters.to_bytes()?, merkle_tree_depth);

    if let Some(cached) = IVC_PROVER_SETUP_CACHE
        .lock()
        .map_err(|_| anyhow!("IVC prover setup cache lock poisoned."))?
        .get(&cache_key)
    {
        return Ok(cached.clone());
    }

    let trusted_setup_provider = TrustedSetupProvider::default();
    let srs = Arc::new(trusted_setup_provider.get_trusted_setup_parameters()?);
    let certificate_key_provider =
        TempCertificateKeyProvider::new(srs.clone(), parameters, merkle_tree_depth);
    let certificate_midnight_verifying_key =
        certificate_key_provider.get_midnight_verifying_key()?;
    let ivc_key_provider =
        TempIvcKeyProvider::new(srs.clone(), certificate_key_provider.get_verifying_key()?);
    let ivc_setup = Arc::new(IvcProverSetup::load(
        &trusted_setup_provider,
        &certificate_key_provider,
        &ivc_key_provider,
    )?);

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
        use std::sync::Arc;

        use midnight_proofs::poly::commitment::Params;
        use tempfile::tempdir;

        use crate::{
            Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    tests::common::{
                        asset_readers::load_embedded_verification_context_asset,
                        generators::setup::{QUORUM_SIZE, SIGNER_COUNT},
                    },
                },
                trusted_setup::build_provider_with_unsafe_srs,
            },
            proof_system::ivc_halo2_snark::{
                IvcProverSetup, TempCertificateKeyProvider, TempIvcKeyProvider,
            },
        };

        // The IVC circuit is degree `K`, but production builds its setup from the larger degree-22
        // SRS. Keygen and the stored proving SRS must both downsize to `K`, otherwise their Lagrange
        // basis differs and proofs do not verify. A larger unsafe SRS shares the smaller one's tau,
        // so a correctly downsized setup reproduces the embedded degree-`K` assets exactly.
        #[test]
        fn ivc_setup_downsizes_keys_and_srs_to_the_circuit_degree() {
            let temp_dir = tempdir().expect("temp dir creation should succeed");
            let trusted_setup_provider = build_provider_with_unsafe_srs(temp_dir.path(), K + 1);
            let srs = Arc::new(
                trusted_setup_provider
                    .get_trusted_setup_parameters()
                    .expect("oversized unsafe SRS should load"),
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
            let ivc_setup =
                IvcProverSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                    .expect("IvcProverSetup::load should succeed");

            let verification_context = load_embedded_verification_context_asset()
                .expect("verification context asset should load");

            assert_eq!(
                verification_context.certificate_verifying_key.vk().transcript_repr(),
                ivc_setup.certificate_verifying_key.transcript_repr(),
                "cert VK must be independent of the SRS degree (downsized at keygen)"
            );
            assert_eq!(
                verification_context.recursive_verifying_key.transcript_repr(),
                ivc_setup.ivc_verifying_key.transcript_repr(),
                "IVC VK must be independent of the SRS degree (downsized at keygen)"
            );
            assert_eq!(
                ivc_setup.srs.max_k(),
                K,
                "the proving SRS stored in IvcProverSetup must be downsized to the IVC circuit degree"
            );
        }
    }
}
