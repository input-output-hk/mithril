//! Temporary unsafe setup helpers used by `IvcSetup::load` until the production
//! cache providers for the recursive circuit are implemented.
//!
//! Each provider stores its prerequisites as owned fields and computes its artifact
//! on demand. The caller orchestrates the chain: first the cert VK provider, then
//! the IVC VK provider (constructed with the computed cert VK), then the IVC PK
//! provider (constructed with both VKs).

// TODO: remove this module once the production IVC cache providers ship.

use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{keygen_pk, keygen_vk_with_k},
    poly::kzg::params::ParamsKZG,
};
use midnight_zk_stdlib::{self as zk};

use crate::{
    Parameters, StmResult,
    circuits::{
        halo2::circuit::StmCertificateCircuit,
        halo2_ivc::{K, circuit::IvcCircuit},
    },
    proof_system::ivc_halo2_snark::{CircuitProvingKey, CircuitVerifyingKey},
};

#[allow(dead_code)]
pub(crate) struct TempCertificateVerifyingKeyProvider {
    srs: ParamsKZG<Bls12>,
    parameters: Parameters,
    merkle_tree_depth: u32,
}

#[allow(dead_code)]
impl TempCertificateVerifyingKeyProvider {
    pub(crate) fn new(
        srs: ParamsKZG<Bls12>,
        parameters: Parameters,
        merkle_tree_depth: u32,
    ) -> Self {
        Self {
            srs,
            parameters,
            merkle_tree_depth,
        }
    }

    pub(crate) fn get_verifying_key(&self) -> StmResult<CircuitVerifyingKey> {
        let cert_circuit =
            StmCertificateCircuit::try_new(&self.parameters, self.merkle_tree_depth)?;
        let mut cert_srs = self.srs.clone();
        zk::downsize_srs_for_relation(&mut cert_srs, &cert_circuit);
        Ok(zk::setup_vk(&cert_srs, &cert_circuit).vk().clone())
    }
}

#[allow(dead_code)]
pub(crate) struct TempIvcVerifyingKeyProvider {
    srs: ParamsKZG<Bls12>,
    certificate_verifying_key: CircuitVerifyingKey,
}

#[allow(dead_code)]
impl TempIvcVerifyingKeyProvider {
    pub(crate) fn new(
        srs: ParamsKZG<Bls12>,
        certificate_verifying_key: CircuitVerifyingKey,
    ) -> Self {
        Self {
            srs,
            certificate_verifying_key,
        }
    }

    pub(crate) fn get_verifying_key(&self) -> StmResult<CircuitVerifyingKey> {
        let ivc_circuit = IvcCircuit::unknown(&self.certificate_verifying_key);
        Ok(keygen_vk_with_k(&self.srs, &ivc_circuit, K)?)
    }
}

#[allow(dead_code)]
pub(crate) struct TempIvcProvingKeyProvider {
    certificate_verifying_key: CircuitVerifyingKey,
    ivc_verifying_key: CircuitVerifyingKey,
}

#[allow(dead_code)]
impl TempIvcProvingKeyProvider {
    pub(crate) fn new(
        certificate_verifying_key: CircuitVerifyingKey,
        ivc_verifying_key: CircuitVerifyingKey,
    ) -> Self {
        Self {
            certificate_verifying_key,
            ivc_verifying_key,
        }
    }

    pub(crate) fn get_proving_key(&self) -> StmResult<CircuitProvingKey> {
        let ivc_circuit = IvcCircuit::unknown(&self.certificate_verifying_key);
        Ok(keygen_pk(self.ivc_verifying_key.clone(), &ivc_circuit)?)
    }
}
