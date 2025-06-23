use kes_summed_ed25519::kes::Sum6KesSig;

use crate::{
    crypto_helper::{KESPeriod, OpCert},
    StdResult,
};

/// Trait for KES (Key Evolving Signature) signing operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesSigner {
    /// Return signed bytes with the KES secret key and the associated Operational Certificate
    fn sign(&self, message: &[u8], kes_period: KESPeriod) -> StdResult<(Sum6KesSig, OpCert)>;
}

/// Trait for KES (Key Evolving Signature) verification operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesVerifier {
    /// Verify the signed message and return the original message.
    fn verify(
        &self,
        message: &[u8],
        signature: &Sum6KesSig,
        operational_certificate: &OpCert,
        kes_period: KESPeriod,
    ) -> StdResult<()>;
}
