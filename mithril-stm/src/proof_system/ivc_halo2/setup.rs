//! Load-once, deployment-constant artifacts shared by every step of an IVC proving session.

use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey},
    poly::kzg::{KZGCommitmentScheme, params::ParamsKZG},
};
use std::collections::BTreeMap;

use crate::circuits::halo2::types::CircuitBase;

/// Load-once, deployment-constant artifacts shared by every step of an IVC proving session.
// TODO: remove this allow dead_code directive when the IVC prover consumes this setup
#[allow(dead_code)]
pub(crate) struct IvcSetup {
    /// KZG structured reference string.
    pub(crate) srs: ParamsKZG<Bls12>,
    /// Verifying key of the certificate circuit.
    pub(crate) certificate_verifying_key: VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
    /// Verifying key of the IVC circuit.
    pub(crate) ivc_verifying_key: VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
    /// Proving key of the IVC circuit.
    pub(crate) ivc_proving_key: ProvingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
    /// Fixed-base map used to normalize the certificate accumulator.
    pub(crate) certificate_fixed_bases: BTreeMap<String, G1Projective>,
    /// Fixed-base map used to normalize the IVC self-proof accumulator.
    pub(crate) ivc_fixed_bases: BTreeMap<String, G1Projective>,
    /// Fixed-base map used when folding the certificate and self-proof accumulators
    /// into the new IVC folded accumulator.
    pub(crate) combined_fixed_bases: BTreeMap<String, G1Projective>,
}
