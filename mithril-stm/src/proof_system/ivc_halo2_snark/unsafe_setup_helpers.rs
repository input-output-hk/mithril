//! Temporary unsafe setup helpers used by `IvcProverSetup::load` until the production
//! cache providers for the recursive circuit are implemented.
//!
//! Two providers, one per circuit, each exposing both `get_verifying_key` and
//! `get_proving_key`. The IVC provider depends on the certificate verifying
//! key: the caller pulls the cert VK from the certificate provider first, then
//! constructs the IVC provider with it.
//!
//! Each provider stores its prerequisites as owned fields and recomputes its
//! artifact on demand. Without a cache, `get_proving_key` recomputes the
//! verifying key internally; that overhead disappears at production swap time.

// TODO: remove this module once the production IVC cache providers ship.

use std::sync::Arc;

use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{keygen_pk, keygen_vk_with_k},
    poly::kzg::params::ParamsKZG,
};
use midnight_zk_stdlib::{self as zk, MidnightVK};

use crate::{
    Parameters, StmResult,
    circuits::{
        halo2::circuit::StmCertificateCircuit,
        halo2_ivc::{K, circuit::IvcCircuitData},
    },
    proof_system::ivc_halo2_snark::{CircuitProvingKey, CircuitVerifyingKey},
};

/// Recomputes the certificate-circuit verifying key and proving key from a
/// shared SRS plus the `Parameters` and Merkle tree depth that pin the circuit
/// shape.
pub(crate) struct TempCertificateKeyProvider {
    /// Shared KZG structured reference string, sized for the IVC circuit. Cloned
    /// on demand and downsized in place before certificate-circuit keygen.
    srs: Arc<ParamsKZG<Bls12>>,
    /// STM protocol parameters that determine the certificate circuit shape.
    parameters: Parameters,
    /// Merkle tree depth used by the certificate circuit.
    merkle_tree_depth: u32,
}

impl TempCertificateKeyProvider {
    /// Stores the prerequisites needed to recompute the certificate keys.
    pub(crate) fn new(
        srs: Arc<ParamsKZG<Bls12>>,
        parameters: Parameters,
        merkle_tree_depth: u32,
    ) -> Self {
        Self {
            srs,
            parameters,
            merkle_tree_depth,
        }
    }

    /// Builds the certificate circuit, clones the shared SRS so it can be
    /// downsized in place, and runs `zk::setup_vk` to produce the [MidnightVK].
    ///
    /// The [MidnightVK] carries the circuit architecture, which is required to
    /// deserialize the verifying key against the correct constraint system.
    pub(crate) fn get_midnight_verifying_key(&self) -> StmResult<MidnightVK> {
        let certificate_circuit =
            StmCertificateCircuit::try_new(&self.parameters, self.merkle_tree_depth)?;
        let mut certificate_srs = (*self.srs).clone();
        zk::downsize_srs_for_relation(&mut certificate_srs, &certificate_circuit);
        Ok(zk::setup_vk(&certificate_srs, &certificate_circuit))
    }

    /// Builds the certificate circuit, clones the shared SRS so it can be
    /// downsized in place, and runs `zk::setup_vk` to produce the verifying key.
    pub(crate) fn get_verifying_key(&self) -> StmResult<CircuitVerifyingKey> {
        Ok(self.get_midnight_verifying_key()?.vk().clone())
    }
}

/// Recomputes the IVC-circuit verifying key and proving key from a shared SRS
/// and the already-computed certificate verifying key (which the IVC circuit
/// recursively verifies).
pub(crate) struct TempIvcKeyProvider {
    /// Shared KZG structured reference string, sized for the IVC circuit.
    srs: Arc<ParamsKZG<Bls12>>,
    /// Certificate verifying key produced by the cert provider; the IVC circuit
    /// is parameterized by this key.
    certificate_verifying_key: CircuitVerifyingKey,
}

impl TempIvcKeyProvider {
    /// Stores the prerequisites needed to recompute the IVC keys.
    pub(crate) fn new(
        srs: Arc<ParamsKZG<Bls12>>,
        certificate_verifying_key: CircuitVerifyingKey,
    ) -> Self {
        Self {
            srs,
            certificate_verifying_key,
        }
    }

    /// Builds an unknown-witness IVC circuit parameterized by the certificate VK
    /// and runs `keygen_vk_with_k` at the IVC circuit's domain size `K`.
    pub(crate) fn get_verifying_key(&self) -> StmResult<CircuitVerifyingKey> {
        let ivc_circuit_data = IvcCircuitData::unknown(&self.certificate_verifying_key)?;
        let mut ivc_srs = (*self.srs).clone();
        ivc_srs.downsize(K);
        Ok(keygen_vk_with_k(&ivc_srs, &ivc_circuit_data, K)?)
    }

    /// Recomputes the IVC verifying key (the temp layer has no cache, so the VK
    /// is re-derived here) and runs `keygen_pk` to produce the proving key.
    pub(crate) fn get_proving_key(&self) -> StmResult<CircuitProvingKey> {
        let ivc_circuit_data = IvcCircuitData::unknown(&self.certificate_verifying_key)?;
        let mut ivc_srs = (*self.srs).clone();
        ivc_srs.downsize(K);
        let ivc_verifying_key = keygen_vk_with_k(&ivc_srs, &ivc_circuit_data, K)?;
        Ok(keygen_pk(ivc_verifying_key, &ivc_circuit_data)?)
    }
}
