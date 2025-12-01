//! Provides queries to retrieve or send data to a Mithril aggregator
//!
//! Available queries
//! - Get:
//!   - Aggregator features: [Get current aggregator features][GetAggregatorFeaturesQuery]
//!   - Cardano database v1 (aka Snapshot): [List][GetSnapshotsListQuery], [Get by hash][GetSnapshotQuery]
//!   - Certificate: [List][GetCertificatesListQuery], [Get by hash, get latest genesis certificate][GetCertificateQuery]
//!   - Epoch settings: [Get current epoch settings][GetEpochSettingsQuery]
//!   - Mithril stake distribution: [List][GetMithrilStakeDistributionsListQuery], [Get by hash][GetMithrilStakeDistributionQuery]
//!   - Protocol Configuration: [Get for a given epoch][GetProtocolConfigurationQuery]
//! - Post:
//!   - Signature: [Register a signature][PostRegisterSignatureQuery]
//!   - Registration: [Send a signer registration][PostRegisterSignerQuery]
//!   - Cardano database v1 (aka Snapshot): [Increment snapshot download statistic][PostIncrementSnapshotDownloadStatisticQuery]
//!   - Cardano database v2:
//!     - Increment Cardano database snapshot [complete or partial restoration statistic][PostIncrementCardanoDatabaseRestorationStatisticQuery]
//!     - Increment Cardano database snapshot [immutables restored statistic][PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery]
//!     - Increment Cardano database snapshot [ancillary files restored statistic][PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery]
//!
mod api;
mod get;
mod post;

pub(crate) use api::*;
pub use get::*;
pub use post::*;
