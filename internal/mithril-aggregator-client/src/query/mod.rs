//! Provides queries to retrieve or send data to a Mithril aggregator
//!
//! Available queries
//! - Get:
//!   - Aggregator features: [Get current aggregator features][GetAggregatorFeaturesQuery]
//!   - Certificate: [List][GetCertificatesListQuery], [Get by hash, get latest genesis certificate][GetCertificateQuery]
//!   - Epoch settings: [Get current epoch settings][GetEpochSettingsQuery]
//!   - Protocol Configuration: [Get for a given epoch][GetProtocolConfigurationQuery]
//! - Post:
//!   - signature: [Register a signature][PostRegisterSignatureQuery]
//!   - registration: [Send a signer registration][PostRegisterSignerQuery]
//!
mod api;
mod get;
mod post;

pub(crate) use api::*;
pub use get::*;
pub use post::*;
