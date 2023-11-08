/// Mithril result type, an alias of [anyhow::Result]
pub type MithrilResult<T> = anyhow::Result<T>;

/// Mithril error type, an alias of [anyhow::Error]
pub type MithrilError = anyhow::Error;

/// A Mithril snapshot of a Cardano Node database.
///
pub use mithril_common::messages::SnapshotMessage as Snapshot;

/// List item of Mithril snapshots
///
pub use mithril_common::messages::SnapshotListItemMessage as SnapshotListItem;

/// A Mithril stake distribution.
///
pub use mithril_common::messages::MithrilStakeDistributionMessage as MithrilStakeDistribution;

/// List item of Mithril stake distribution.
///
pub use mithril_common::messages::MithrilStakeDistributionListItemMessage as MithrilStakeDistributionListItem;

/// A Mithril certificate.
///
pub use mithril_common::messages::CertificateMessage as MithrilCertificate;

pub use mithril_common::messages::CertificateMetadataMessagePart as MithrilCertificateMetadata;

/// List item of certificates
///
pub use mithril_common::messages::CertificateListItemMessage as MithrilCertificateListItem;

pub use mithril_common::messages::CertificateListItemMessageMetadata as MithrilCertificateListItemMetadata;

/// A individual signer of a [mithril certificate][MithrilCertificate]
///
pub use mithril_common::messages::SignerWithStakeMessagePart as MithrilSigner;

/// `mithril-common` re-exports
pub mod common {
    pub use mithril_common::entities::{
        Beacon, CompressionAlgorithm, Epoch, ProtocolMessage, ProtocolMessagePartKey,
        ProtocolParameters,
    };
}
