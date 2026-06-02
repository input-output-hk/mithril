//! Load-once, deployment-constant artifacts shared by every step of an IVC proving session.

use std::collections::BTreeMap;

use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::poly::kzg::params::ParamsKZG;

use crate::{
    StmResult,
    circuits::{
        halo2_ivc::{CERT_VK_NAME, IVC_ONE_NAME, state::fixed_bases_and_names},
        trusted_setup::TrustedSetupProvider,
    },
    proof_system::ivc_halo2_snark::{
        CircuitProvingKey, CircuitVerifyingKey,
        unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider},
    },
};

/// Load-once, deployment-constant artifacts shared by every step of an IVC proving session.
///
/// # Invariants
///
/// The three fixed-base maps are not independent. The constructor must enforce:
///
/// `combined_fixed_bases.keys() == certificate_fixed_bases.keys() ∪ ivc_fixed_bases.keys()`
///
/// and values must agree across the three maps for any shared key. The in-circuit IVC
/// verifier gadget builds a single merged fixed-base list from these names; any mismatch
/// here produces folded accumulators the circuit will reject.
// Kept until the IVC prover loads production setup artifacts.
#[allow(dead_code)]
pub(crate) struct IvcSetup {
    /// KZG structured reference string.
    pub(crate) srs: ParamsKZG<Bls12>,
    /// Verifying key of the certificate circuit.
    pub(crate) certificate_verifying_key: CircuitVerifyingKey,
    /// Verifying key of the IVC circuit.
    pub(crate) ivc_verifying_key: CircuitVerifyingKey,
    /// Proving key of the IVC circuit.
    pub(crate) ivc_proving_key: CircuitProvingKey,
    /// Fixed-base map used to normalize the certificate accumulator.
    pub(crate) certificate_fixed_bases: BTreeMap<String, G1Projective>,
    /// Fixed-base map used to normalize the IVC self-proof accumulator.
    pub(crate) ivc_fixed_bases: BTreeMap<String, G1Projective>,
    /// Fixed-base map used when folding the certificate and self-proof accumulators
    /// into the new IVC folded accumulator.
    pub(crate) combined_fixed_bases: BTreeMap<String, G1Projective>,
}

#[allow(dead_code)]
impl IvcSetup {
    /// Derives the full IVC setup by orchestrating the key providers.
    ///
    /// The function pulls the SRS, the certificate verifying key, the IVC verifying key,
    /// and the IVC proving key from the supplied providers, extracts the three fixed-base
    /// maps from the verifying keys, and assembles them into an `IvcSetup`.
    ///
    /// Providers are currently the temporary pure-compute ones in
    /// `unsafe_setup_helpers`; they share the API surface the production cache providers
    /// will expose. When the cache work lands, the temp provider types in this signature
    /// are replaced with the real cache providers; the body stays unchanged.
    ///
    /// # SRS consistency
    ///
    /// The SRS and the key providers are sourced independently by design (today the temp
    /// providers store an `Arc<ParamsKZG<Bls12>>` field; the production cache providers
    /// will not). The caller must ensure the SRS yielded by `trusted_setup_provider`
    /// matches the SRS that produced the key artifacts the providers return. A mismatch
    /// produces an internally inconsistent `IvcSetup` that surfaces only at proof
    /// generation or verification time. In practice: route all of them through the same
    /// `TrustedSetupProvider` instance (temp design) or the same canonical trusted setup
    /// source (production cache design).
    // TODO: swap `Temp*Provider` parameters for the production IVC cache providers
    // once they ship.
    pub(crate) fn load(
        trusted_setup_provider: &TrustedSetupProvider,
        certificate_key_provider: &TempCertificateKeyProvider,
        ivc_key_provider: &TempIvcKeyProvider,
    ) -> StmResult<Self> {
        let srs = trusted_setup_provider.get_trusted_setup_parameters()?;

        let certificate_verifying_key = certificate_key_provider.get_verifying_key()?;
        let ivc_verifying_key = ivc_key_provider.get_verifying_key()?;
        let ivc_proving_key = ivc_key_provider.get_proving_key()?;

        let (certificate_fixed_bases, _) =
            fixed_bases_and_names(CERT_VK_NAME, &certificate_verifying_key);
        let (ivc_fixed_bases, _) = fixed_bases_and_names(IVC_ONE_NAME, &ivc_verifying_key);
        let mut combined_fixed_bases = certificate_fixed_bases.clone();
        combined_fixed_bases.extend(ivc_fixed_bases.clone());

        Ok(Self {
            srs,
            certificate_verifying_key,
            ivc_verifying_key,
            ivc_proving_key,
            certificate_fixed_bases,
            ivc_fixed_bases,
            combined_fixed_bases,
        })
    }
}

// Tests construct the temp providers directly: small `Parameters`, a small Merkle depth,
// and a K=19 SRS produced via `unsafe_setup`. This keeps the slow keygen tractable while
// the production cache providers are not yet wired in.
//
// When the real IVC cache providers ship, these tests will be rewritten end-to-end:
// the temp provider constructions are replaced with the real provider constructions,
// the K=19 unsafe SRS is replaced with the production K=22 SRS loaded through
// `TrustedSetupProvider`, and `IvcSetup::load` is called with the real
// `RecursiveCircuit{Verifying,Proving}KeyProvider` plus the cert
// `CircuitVerificationKeyProvider`. The body of `IvcSetup::load` stays unchanged across
// that swap; only the call site (here) and the provider types change.
#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use tempfile::tempdir;

    use super::*;
    use crate::{
        Parameters,
        circuits::{halo2_ivc::K, trusted_setup::build_provider_with_unsafe_srs},
    };

    // Generates a K=19 SRS and runs `keygen_pk` for the IVC circuit; runs in the
    // `slow` tier (invoke with `cargo test slow::`).
    //
    // TODO: once the production IVC cache providers ship, rewrite this test to use
    // them (warm caches eliminate SRS generation and VK/PK keygen). The rewritten
    // test should run in the default tier rather than `mod slow`.
    mod slow {
        use super::*;

        #[test]
        fn load_succeeds_with_unsafe_srs() {
            let temp_dir = tempdir().unwrap();
            let trusted_setup_provider = build_provider_with_unsafe_srs(temp_dir.path(), K);
            let srs = Arc::new(trusted_setup_provider.get_trusted_setup_parameters().unwrap());

            let parameters = Parameters {
                k: 3,
                m: 10,
                phi_f: 0.2,
            };
            let merkle_tree_depth = 4;

            let certificate_key_provider =
                TempCertificateKeyProvider::new(Arc::clone(&srs), parameters, merkle_tree_depth);
            let certificate_verifying_key = certificate_key_provider.get_verifying_key().unwrap();
            let ivc_key_provider = TempIvcKeyProvider::new(srs, certificate_verifying_key);

            let setup = IvcSetup::load(
                &trusted_setup_provider,
                &certificate_key_provider,
                &ivc_key_provider,
            )
            .unwrap();

            assert!(
                !setup.certificate_fixed_bases.is_empty(),
                "certificate fixed bases should be populated"
            );
            assert!(
                !setup.ivc_fixed_bases.is_empty(),
                "IVC fixed bases should be populated"
            );

            for (key, value) in &setup.certificate_fixed_bases {
                assert_eq!(
                    setup.combined_fixed_bases.get(key),
                    Some(value),
                    "combined map should preserve every certificate base"
                );
            }
            for (key, value) in &setup.ivc_fixed_bases {
                assert_eq!(
                    setup.combined_fixed_bases.get(key),
                    Some(value),
                    "combined map should preserve every IVC base"
                );
            }
        }
    }
}
