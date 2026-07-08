//! Aggregator specific certificate chain validation errors

use thiserror::Error;

use mithril_common::entities::Epoch;

/// Error raised when the local certificate chain has an epoch gap.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
#[error(
    "There is an epoch gap between the last certificate epoch ({certificate_epoch:?}) and current epoch ({current_epoch:?}). A leader aggregator must be re-genesis by the owner of the genesis keys, a follower aggregator will automatically catchup with the leader's certificate chain."
)]
pub struct CertificateEpochGap {
    /// Epoch of the last issued certificate.
    pub certificate_epoch: Epoch,

    /// Given current epoch.
    pub current_epoch: Epoch,
}
