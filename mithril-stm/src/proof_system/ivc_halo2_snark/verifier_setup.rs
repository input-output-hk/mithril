//! Stabilized KZG verification parameters for the IVC proof system.
//!
//! [`IvcVerifierSetup`] bundles everything needed to verify an IVC proof without
//! loading the full KZG SRS (hundreds of MB). The KZG verifier parameters are
//! embedded as a compile-time constant and deserialized on construction.

use std::collections::BTreeMap;

use anyhow::Context;
use midnight_curves::{Bls12, G1Projective, G2Affine};
use midnight_proofs::{poly::kzg::params::ParamsVerifierKZG, utils::SerdeFormat};
use serde::{Deserialize, Serialize};

use crate::{
    SchnorrVerificationKey, StmResult,
    circuits::{
        halo2::keys::NonRecursiveCircuitVerifyingKey,
        halo2_ivc::{
            CERTIFICATE_VERIFICATION_KEY_NAME, IVC_VERIFICATION_KEY_NAME,
            keys::RecursiveCircuitVerifyingKey, state::fixed_bases_and_names, types::MessageHash,
        },
    },
    codec,
    proof_system::{KZG_VERIFIER_PARAMS, ivc_halo2_snark::prover_setup::IvcSnarkProverSetup},
};
// The raw PLONK verifying key is only needed by the test-only `from_parts`, which bridges the
// stored (raw) verification-context asset into the newtype-typed setup.
#[cfg(test)]
use crate::circuits::halo2_ivc::PlonkVerifyingKey;

/// Minimal setup artifacts needed to verify IVC proofs without loading the full SRS.
///
/// Unlike [`IvcSnarkProverSetup`], this struct does not hold a `ParamsKZG` (hundreds of MB). The KZG
/// verifier parameters are embedded as a compile-time constant and deserialized on
/// construction. The caller must supply the verifying keys because the certificate VK varies
/// per deployment.
///
/// # Invariant
///
/// The certificate and IVC verifying keys used to build this struct must match those used to
/// build the [`Global`] passed to [`IvcProof::verify`]. A mismatch silently produces wrong
/// public inputs and will cause verification to fail with
/// [`IvcProofError::KzgOpeningFailed`].
///
/// [`Global`]: crate::circuits::halo2_ivc::state::Global
/// [`IvcProof::verify`]: crate::proof_system::ivc_halo2_snark::proof::IvcProof::verify
/// [`IvcProofError::KzgOpeningFailed`]: crate::proof_system::ivc_halo2_snark::errors::IvcProofError::KzgOpeningFailed
pub(crate) struct IvcVerifierSetup {
    /// Stabilized KZG verifier parameters (embedded constant, no SRS load required).
    verifier_params: ParamsVerifierKZG<Bls12>,
    /// `s_g2` (tau·G2) extracted from the embedded params; passed to the accumulator check.
    tau_g2: G2Affine,
    /// Verifying key of the IVC circuit.
    ivc_verifying_key: RecursiveCircuitVerifyingKey,
    /// Combined fixed-base map (certificate ∪ IVC) used by the accumulator check.
    combined_fixed_bases: BTreeMap<String, G1Projective>,
}

impl IvcVerifierSetup {
    /// Build from the embedded KZG params constant plus caller-supplied verifying keys.
    ///
    /// The certificate VK varies per deployment (k, m, merkle_depth); the IVC VK is typically
    /// deserialized from `RECURSIVE_CIRCUIT_VERIFICATION_KEY_FOR_PRODUCTION`. No SRS needed.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn try_new(
        certificate_verifying_key: &NonRecursiveCircuitVerifyingKey,
        ivc_verifying_key: &RecursiveCircuitVerifyingKey,
    ) -> StmResult<Self> {
        let (verifier_params, tau_g2) = Self::read_embedded_params()?;

        let (certificate_fixed_bases, _) = fixed_bases_and_names(
            CERTIFICATE_VERIFICATION_KEY_NAME,
            certificate_verifying_key.midnight_vk().vk(),
        );
        let (ivc_fixed_bases, _) =
            fixed_bases_and_names(IVC_VERIFICATION_KEY_NAME, ivc_verifying_key.verifying_key());
        let mut combined_fixed_bases = certificate_fixed_bases;
        combined_fixed_bases.extend(ivc_fixed_bases);

        Ok(Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: ivc_verifying_key.clone(),
            combined_fixed_bases,
        })
    }

    /// Derive from an already-built [`IvcSnarkProverSetup`], reusing its precomputed fixed bases.
    ///
    /// Avoids recomputing fixed bases from scratch when a proving session is already running.
    #[allow(dead_code)]
    pub(crate) fn from_ivc_setup(ivc_setup: &IvcSnarkProverSetup) -> StmResult<Self> {
        let (verifier_params, tau_g2) = Self::read_embedded_params()?;
        Ok(Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: ivc_setup.ivc_verifying_key.clone(),
            combined_fixed_bases: ivc_setup.combined_fixed_bases.clone(),
        })
    }

    /// Derive from an already-built [`IvcSnarkProverSetup`], extracting verifier params directly
    /// from its SRS rather than the embedded constant. Only for tests — production code must
    /// not load the full SRS just to verify a proof.
    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn from_ivc_setup_with_srs(ivc_setup: &IvcSnarkProverSetup) -> Self {
        let verifier_params = ivc_setup.srs.verifier_params();
        let tau_g2: G2Affine = verifier_params.s_g2().into();
        Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: ivc_setup.ivc_verifying_key.clone(),
            combined_fixed_bases: ivc_setup.combined_fixed_bases.clone(),
        }
    }

    /// Construct directly from pre-built parts. Only for tests that load stored assets
    /// (verifier params, tau_g2, VK, fixed bases) as a bundle — the caller is responsible
    /// for ensuring `tau_g2 == s_g2()` of `verifier_params` and that `combined_fixed_bases`
    /// covers both the certificate and IVC verifying keys.
    #[cfg(test)]
    pub(crate) fn from_parts(
        verifier_params: ParamsVerifierKZG<Bls12>,
        tau_g2: G2Affine,
        ivc_verifying_key: PlonkVerifyingKey,
        combined_fixed_bases: BTreeMap<String, G1Projective>,
    ) -> Self {
        Self {
            verifier_params,
            tau_g2,
            ivc_verifying_key: RecursiveCircuitVerifyingKey::new(ivc_verifying_key),
            combined_fixed_bases,
        }
    }

    /// Deserialize the compile-time [`KZG_VERIFIER_PARAMS`] constant and extract `tau_g2` (`s_g2`).
    ///
    /// Returns `(verifier_params, tau_g2)` as a pair so callers can use them independently
    /// without re-reading the constant. Shared by [`try_new`] and [`from_ivc_setup`].
    ///
    /// [`try_new`]: Self::try_new
    /// [`from_ivc_setup`]: Self::from_ivc_setup
    pub(crate) fn read_embedded_params() -> StmResult<(ParamsVerifierKZG<Bls12>, G2Affine)> {
        let verifier_params = ParamsVerifierKZG::<Bls12>::read(
            &mut &KZG_VERIFIER_PARAMS[..],
            SerdeFormat::RawBytesUnchecked,
        )
        .with_context(|| "Failed to read embedded IVC verifier params")?;

        let tau_g2: G2Affine = verifier_params.s_g2().into();

        Ok((verifier_params, tau_g2))
    }

    /// Returns the embedded KZG verifier parameters.
    pub(crate) fn verifier_params(&self) -> &ParamsVerifierKZG<Bls12> {
        &self.verifier_params
    }

    /// Returns `tau·G2` (`s_g2`) extracted from the embedded verifier params.
    pub(crate) fn tau_g2(&self) -> &G2Affine {
        &self.tau_g2
    }

    /// Returns the IVC circuit verifying key.
    pub(crate) fn ivc_verifying_key(&self) -> &RecursiveCircuitVerifyingKey {
        &self.ivc_verifying_key
    }

    /// Returns the combined fixed-base map (certificate ∪ IVC) used by the accumulator check.
    pub(crate) fn combined_fixed_bases(&self) -> &BTreeMap<String, G1Projective> {
        &self.combined_fixed_bases
    }
}

/// Represent the data needed by the verifier in order to verify an IVC proof. It contains
/// genesis information and circuit verification keys.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IvcVerifierData {
    genesis_message: MessageHash,
    genesis_schnorr_verification_key: SchnorrVerificationKey,
    certificate_circuit_verification_key: NonRecursiveCircuitVerifyingKey,
    ivc_circuit_verification_key: RecursiveCircuitVerifyingKey,
}

impl IvcVerifierData {
    /// Build the verifier data from the genesis message, the genesis Schnorr verification key and
    /// the certificate and IVC circuit verifying keys used to produce the proof.
    ///
    /// The verifying keys are the per-circuit newtypes so their serialization preserves the circuit
    /// architecture needed to deserialize them against the correct constraint system.
    pub(crate) fn new(
        genesis_message: MessageHash,
        genesis_schnorr_verification_key: SchnorrVerificationKey,
        certificate_circuit_verification_key: NonRecursiveCircuitVerifyingKey,
        ivc_circuit_verification_key: RecursiveCircuitVerifyingKey,
    ) -> Self {
        Self {
            genesis_message,
            genesis_schnorr_verification_key,
            certificate_circuit_verification_key,
            ivc_circuit_verification_key,
        }
    }

    /// Serialize to versioned CBOR bytes, following `CODEC.md`.
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        codec::to_cbor_bytes(self)
    }

    /// Deserialize from versioned CBOR bytes, following `CODEC.md`.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if codec::has_cbor_v1_prefix(bytes) {
            codec::from_cbor_bytes(&bytes[1..])
        } else {
            Err(anyhow::anyhow!(
                "IvcVerifierData: unsupported encoding, expected a CBOR v1 prefix"
            ))
        }
    }

    /// Returns the genesis message (hash of the genesis preimage converted to a field element)
    /// stored in the IvcVerifierData
    pub(crate) fn genesis_message(&self) -> MessageHash {
        self.genesis_message
    }

    /// Returns the genesis schnorr verification key stored in the IvcVerifierData
    pub(crate) fn genesis_schnorr_verification_key(&self) -> SchnorrVerificationKey {
        self.genesis_schnorr_verification_key
    }

    /// Returns the certificate circuit verifying key stored in the IvcVerifierData
    pub(crate) fn certificate_circuit_verification_key(&self) -> &NonRecursiveCircuitVerifyingKey {
        &self.certificate_circuit_verification_key
    }

    /// Returns the ivc circuit verifying key stored in the IvcVerifierData
    pub(crate) fn ivc_circuit_verification_key(&self) -> &RecursiveCircuitVerifyingKey {
        &self.ivc_circuit_verification_key
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::{
        halo2_ivc::tests::common::asset_readers::load_embedded_verification_context_asset,
        trusted_setup::TrustedSetupProvider,
    };

    #[test]
    fn ivc_verifier_data_round_trips_byte_for_byte() {
        let context = load_embedded_verification_context_asset()
            .expect("verification context asset should load");

        let verifier_data = IvcVerifierData::new(
            MessageHash::ZERO,
            SchnorrVerificationKey::default(),
            NonRecursiveCircuitVerifyingKey::new(context.certificate_verifying_key),
            RecursiveCircuitVerifyingKey::new(context.recursive_verifying_key),
        );

        let bytes = verifier_data.to_bytes().expect("serialization should not fail");
        let restored =
            IvcVerifierData::from_bytes(&bytes).expect("deserialization should not fail");
        let reencoded = restored.to_bytes().expect("re-serialization should not fail");

        assert_eq!(
            bytes, reencoded,
            "IvcVerifierData must round-trip byte-for-byte so the aggregator and client compute the same certificate hash"
        );
    }

    /// The verifying-key newtypes must not change the `IvcVerifierData` wire format. This mirrors the
    /// bare-key shape (a `MidnightVK` and a `PlonkVerifyingKey`, each via a raw-bytes `#[serde(with)]`
    /// module), serializes it through the same codec, then asserts the current struct decodes those
    /// bytes and re-encodes them byte-for-byte.
    mod wire_format_compatibility {
        use midnight_curves::Bls12;
        use midnight_proofs::utils::SerdeFormat;
        use midnight_proofs::{plonk::VerifyingKey, poly::kzg::KZGCommitmentScheme};
        use midnight_zk_stdlib::MidnightVK;
        use serde::{Deserialize, Serialize};

        use super::super::IvcVerifierData;
        use crate::SchnorrVerificationKey;
        use crate::circuits::halo2::types::CircuitBase;
        use crate::circuits::halo2_ivc::circuit::IvcCircuitData;
        use crate::circuits::halo2_ivc::tests::common::asset_readers::load_embedded_verification_context_asset;
        use crate::circuits::halo2_ivc::types::MessageHash;
        use crate::codec;

        mod certificate_verifying_key_serde {
            use serde::{Deserializer, Serializer};

            use super::{MidnightVK, SerdeFormat};

            pub fn serialize<S: Serializer>(vk: &MidnightVK, s: S) -> Result<S::Ok, S::Error> {
                let mut buf = Vec::new();
                vk.write(&mut buf, SerdeFormat::RawBytes)
                    .map_err(serde::ser::Error::custom)?;
                s.serialize_bytes(&buf)
            }

            pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<MidnightVK, D::Error> {
                let bytes: Vec<u8> = serde::Deserialize::deserialize(d)?;
                MidnightVK::read(&mut bytes.as_slice(), SerdeFormat::RawBytes)
                    .map_err(serde::de::Error::custom)
            }
        }

        mod ivc_verifying_key_serde {
            use serde::{Deserializer, Serializer};

            use super::{
                Bls12, CircuitBase, IvcCircuitData, KZGCommitmentScheme, SerdeFormat, VerifyingKey,
            };

            pub fn serialize<S: Serializer>(
                vk: &VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
                s: S,
            ) -> Result<S::Ok, S::Error> {
                let mut buf = Vec::new();
                vk.write(&mut buf, SerdeFormat::RawBytes)
                    .map_err(serde::ser::Error::custom)?;
                s.serialize_bytes(&buf)
            }

            pub fn deserialize<'de, D: Deserializer<'de>>(
                d: D,
            ) -> Result<VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>, D::Error>
            {
                let bytes: Vec<u8> = serde::Deserialize::deserialize(d)?;
                VerifyingKey::<CircuitBase, KZGCommitmentScheme<Bls12>>::read::<_, IvcCircuitData>(
                    &mut bytes.as_slice(),
                    SerdeFormat::RawBytes,
                    (),
                )
                .map_err(serde::de::Error::custom)
            }
        }

        #[derive(Serialize, Deserialize)]
        struct BareKeyIvcVerifierData {
            genesis_message: MessageHash,
            genesis_schnorr_verification_key: SchnorrVerificationKey,
            #[serde(with = "certificate_verifying_key_serde")]
            certificate_circuit_verification_key: MidnightVK,
            #[serde(with = "ivc_verifying_key_serde")]
            ivc_circuit_verification_key: VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
        }

        #[test]
        fn bare_key_shape_round_trips_through_current_struct() {
            let context = load_embedded_verification_context_asset()
                .expect("verification context asset should load");
            let old = BareKeyIvcVerifierData {
                genesis_message: MessageHash::ZERO,
                genesis_schnorr_verification_key: SchnorrVerificationKey::default(),
                certificate_circuit_verification_key: context.certificate_verifying_key.clone(),
                ivc_circuit_verification_key: context.recursive_verifying_key.clone(),
            };
            let old_bytes =
                codec::to_cbor_bytes(&old).expect("bare-key-shape serialization should not fail");

            let current = IvcVerifierData::from_bytes(&old_bytes)
                .expect("the current struct must decode the bare-key-shape bytes");
            let reencoded = current.to_bytes().expect("re-serialization should not fail");

            assert_eq!(
                old_bytes, reencoded,
                "the verifying-key newtypes must preserve the IvcVerifierData wire format byte-for-byte"
            );
        }
    }

    #[test]
    fn try_new_merges_certificate_and_ivc_fixed_bases() {
        let ctx = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let certificate_verifying_key =
            NonRecursiveCircuitVerifyingKey::new(ctx.certificate_verifying_key);
        let recursive_verifying_key =
            RecursiveCircuitVerifyingKey::new(ctx.recursive_verifying_key);
        let setup = IvcVerifierSetup::try_new(&certificate_verifying_key, &recursive_verifying_key)
            .expect("try_new must succeed with valid verifying keys");
        assert_eq!(
            setup.combined_fixed_bases.keys().collect::<Vec<_>>(),
            ctx.combined_fixed_bases.keys().collect::<Vec<_>>(),
            "combined_fixed_bases keys must match the stored verification context"
        );
    }

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
