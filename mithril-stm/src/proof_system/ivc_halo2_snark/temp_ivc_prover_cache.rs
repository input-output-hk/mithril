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
