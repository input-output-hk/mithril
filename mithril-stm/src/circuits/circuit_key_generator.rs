// Lets each circuit derive its own verifying and proving keys from an SRS, so the verification key
// provider can compute keys on a cache miss without a separate generator type.
use midnight_curves::Bls12;
use midnight_proofs::poly::kzg::params::ParamsKZG;

use crate::{
    StmResult,
    codec::{TryFromBytes, TryToBytes},
};

/// A circuit that can generate its verifying and proving keys from a sufficiently sized SRS.
pub(crate) trait CircuitKeyGenerator {
    /// Verifying key; round-trips through bytes and is cloneable for caching.
    type VerifyingKey: TryFromBytes + TryToBytes + Clone;
    /// Proving key; round-trips through bytes.
    type ProvingKey: TryFromBytes + TryToBytes;

    /// Derives the verifying and proving keys from `srs`, downsizing a clone of `srs` to the
    /// circuit's own relation degree and never mutating the caller's SRS.
    fn generate_key_pair(
        &self,
        srs: &ParamsKZG<Bls12>,
    ) -> StmResult<(Self::VerifyingKey, Self::ProvingKey)>;
}
