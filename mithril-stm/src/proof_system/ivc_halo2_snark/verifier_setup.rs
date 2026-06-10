//! Stabilized KZG verification parameters for the IVC proof system.
//!
//! [`IvcVerifierSetup`] bundles everything needed to verify an IVC proof without
//! loading the full KZG SRS (hundreds of MB). The KZG verifier parameters are
//! embedded as a compile-time constant and deserialized on construction.

use std::collections::BTreeMap;

use anyhow::Context;
use midnight_curves::{Bls12, G1Projective, G2Affine};
use midnight_proofs::{poly::kzg::params::ParamsVerifierKZG, utils::SerdeFormat};

use crate::{
    StmResult,
    circuits::halo2_ivc::{
        CERTIFICATE_VERIFICATION_KEY_NAME, IVC_VERIFICATION_KEY_NAME, state::fixed_bases_and_names,
    },
    proof_system::ivc_halo2_snark::{CircuitVerifyingKey, setup::IvcSetup},
};

use crate::proof_system::KZG_VERIFIER_PARAMS;

/// Minimal setup artifacts needed to verify IVC proofs without loading the full SRS.
///
/// Unlike [`IvcSetup`], this struct does not hold a `ParamsKZG` (hundreds of MB). The KZG
/// verifier parameters are embedded as a compile-time constant and deserialized on
/// construction. The caller must supply the verifying keys because the certificate VK varies
/// per deployment.
// TODO: remove this allow dead_code directive when IvcVerifierSetup is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcVerifierSetup {
    /// Stabilized KZG verifier parameters (embedded constant, no SRS load required).
    pub(crate) verifier_params: ParamsVerifierKZG<Bls12>,
    /// `s_g2` (tau·G2) extracted from the embedded params; passed to the accumulator check.
    pub(crate) tau_g2: G2Affine,
    /// Verifying key of the IVC circuit.
    pub(crate) ivc_verifying_key: CircuitVerifyingKey,
    /// Combined fixed-base map (certificate ∪ IVC) used by the accumulator check.
    pub(crate) combined_fixed_bases: BTreeMap<String, G1Projective>,
}

#[allow(dead_code)]
impl IvcVerifierSetup {
    /// Build from the embedded KZG params constant plus caller-supplied verifying keys.
    ///
    /// The certificate VK varies per deployment (k, m, merkle_depth); the IVC VK is typically
    /// deserialized from `RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION`. No SRS needed.
    pub(crate) fn try_new(
        certificate_verifying_key: &CircuitVerifyingKey,
        ivc_verifying_key: CircuitVerifyingKey,
    ) -> StmResult<Self> {
        let (verifier_params, tau_g2) = Self::read_embedded_params()?;

        let (certificate_fixed_bases, _) =
            fixed_bases_and_names(CERTIFICATE_VERIFICATION_KEY_NAME, certificate_verifying_key);
        let (ivc_fixed_bases, _) =
            fixed_bases_and_names(IVC_VERIFICATION_KEY_NAME, &ivc_verifying_key);
        let mut combined_fixed_bases = certificate_fixed_bases;
        combined_fixed_bases.extend(ivc_fixed_bases);

        Ok(Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key,
            combined_fixed_bases,
        })
    }

    /// Derive from an already-built [`IvcSetup`], reusing its precomputed fixed bases.
    ///
    /// Avoids recomputing fixed bases from scratch when a proving session is already running.
    pub(crate) fn from_ivc_setup(ivc_setup: &IvcSetup) -> StmResult<Self> {
        let (verifier_params, tau_g2) = Self::read_embedded_params()?;
        Ok(Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: ivc_setup.ivc_verifying_key.clone(),
            combined_fixed_bases: ivc_setup.combined_fixed_bases.clone(),
        })
    }

    /// Derive from an already-built [`IvcSetup`], extracting verifier params directly from its
    /// SRS rather than the embedded constant. Only for tests — production code must not load the
    /// full SRS just to verify a proof.
    #[cfg(test)]
    pub(crate) fn from_ivc_setup_with_srs(ivc_setup: &IvcSetup) -> Self {
        let verifier_params = ivc_setup.srs_verifier_params.clone();
        let tau_g2: G2Affine = verifier_params.s_g2().into();
        Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: ivc_setup.ivc_verifying_key.clone(),
            combined_fixed_bases: ivc_setup.combined_fixed_bases.clone(),
        }
    }

    pub(crate) fn read_embedded_params() -> StmResult<(ParamsVerifierKZG<Bls12>, G2Affine)> {
        let verifier_params = ParamsVerifierKZG::<Bls12>::read(
            &mut &KZG_VERIFIER_PARAMS[..],
            SerdeFormat::RawBytesUnchecked,
        )
        .with_context(|| "Failed to read embedded IVC verifier params")?;

        let tau_g2: G2Affine = verifier_params.s_g2().into();

        Ok((verifier_params, tau_g2))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::trusted_setup::TrustedSetupProvider;

    #[test]
    fn embedded_ivc_verifier_params_deserialize_without_error() {
        IvcVerifierSetup::read_embedded_params()
            .expect("embedded IVC verifier params bytes must deserialize successfully");
    }

    #[test]
    #[ignore = "requires SRS download from the internet"]
    fn ivc_verifier_params_match_trusted_srs() {
        let srs = TrustedSetupProvider::default()
            .get_trusted_setup_parameters()
            .unwrap();
        let expected = srs.verifier_params();
        let mut expected_bytes = vec![];
        expected
            .write(&mut expected_bytes, SerdeFormat::RawBytesUnchecked)
            .unwrap();
        assert_eq!(
            expected_bytes.as_slice(),
            &KZG_VERIFIER_PARAMS[..],
            "embedded IVC verifier params must match the Midnight trusted SRS"
        );
    }
}
