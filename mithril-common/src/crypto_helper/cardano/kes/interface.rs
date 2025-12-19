use std::fmt::Debug;

use kes_summed_ed25519::kes::Sum6KesSig;

use crate::{
    StdResult,
    crypto_helper::{KesEvolutions, KesPeriod, OpCert},
};

/// Trait for KES (Key Evolving Signature) signing operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesSigner: Send + Sync {
    /// Return signed bytes with the KES secret key and the associated Operational Certificate
    ///
    /// current_kes_period: The KES period used to sign the message (absolute period computed from the chain at the moment of signature)
    fn sign(
        &self,
        message: &[u8],
        current_kes_period: KesPeriod,
    ) -> StdResult<(Sum6KesSig, OpCert)>;
}

/// Trait for KES (Key Evolving Signature) verification operation.
#[cfg_attr(test, mockall::automock)]
pub trait KesVerifier: Send + Sync + Debug {
    /// Verify the signed message and return the original message.
    ///
    /// kes_evolutions: The KES evolutions used to verify the signature (computed from the current KES period at the time of signing minus the start KES period in the operational certificate)
    fn verify(
        &self,
        message: &[u8],
        signature: &Sum6KesSig,
        operational_certificate: &OpCert,
        kes_evolutions: KesEvolutions,
    ) -> StdResult<()>;
}
