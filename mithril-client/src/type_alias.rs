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

/// List item of Mithril stake distributions.
///
pub use mithril_common::messages::MithrilStakeDistributionListItemMessage as MithrilStakeDistributionListItem;

/// A Mithril certificate.
///
pub use mithril_common::messages::CertificateMessage as MithrilCertificate;

pub use mithril_common::messages::CertificateMetadataMessagePart as MithrilCertificateMetadata;

/// List item of Mithril certificates
///
pub use mithril_common::messages::CertificateListItemMessage as MithrilCertificateListItem;

pub use mithril_common::messages::CertificateListItemMessageMetadata as MithrilCertificateListItemMetadata;

/// An individual signer of a [Mithril certificate][MithrilCertificate]
///
pub use mithril_common::messages::SignerWithStakeMessagePart as MithrilSigner;

cfg_unstable! {
    pub use mithril_common::messages::CardanoTransactionsProofsMessage as CardanoTransactionsProofs;

    pub use mithril_common::messages::CardanoTransactionsSetProofMessagePart as CardanoTransactionsSetProof;

    pub use mithril_common::messages::VerifiedCardanoTransactions;

    pub use mithril_common::messages::VerifyCardanoTransactionsProofsError;

    /// A snapshot that allow to know up to which [point of time][common::CardanoDbBeacon] Mithril have certified Cardano transactions.
    ///
    pub use mithril_common::messages::CardanoTransactionSnapshotMessage as CardanoTransactionSnapshot;

    /// List item of a Cardano transaction snapshot.
    ///
    pub use mithril_common::messages::CardanoTransactionSnapshotListItemMessage as CardanoTransactionSnapshotListItem;
}

/// `mithril-common` re-exports
pub mod common {
    pub use mithril_common::entities::{
        CardanoDbBeacon, CompressionAlgorithm, Epoch, ImmutableFileNumber, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters,
    };
    cfg_unstable! {
        pub use mithril_common::entities::{ChainPoint, TransactionHash, SlotNumber, BlockHash, BlockNumber};
    }
}
