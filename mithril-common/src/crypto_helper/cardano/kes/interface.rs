use std::fmt::Debug;

use kes_summed_ed25519::kes::Sum6KesSig;

use crate::{
    StdResult,
    crypto_helper::{KesPeriod, OpCert},
};

/// Trait for KES (Key Evolving Signature) signing operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesSigner: Send + Sync {
    /// Return signed bytes with the KES secret key and the associated Operational Certificate
    fn sign(&self, message: &[u8], kes_period: KesPeriod) -> StdResult<(Sum6KesSig, OpCert)>;
}

/// Trait for KES (Key Evolving Signature) verification operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesVerifier: Send + Sync + Debug {
    /// Verify the signed message and return the original message.
    fn verify(
        &self,
        message: &[u8],
        signature: &Sum6KesSig,
        operational_certificate: &OpCert,
        kes_period: KesPeriod,
    ) -> StdResult<()>;
}
