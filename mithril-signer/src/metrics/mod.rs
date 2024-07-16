//! metrics module.
//! This module contains the signer metrics service and metrics server.

mod server;
mod service;

pub use server::MetricsServer;
pub use service::MetricsService;

/// 'signer_registration_success_since_startup' metric name
pub const SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_signer_registration_success_since_startup";
/// 'signer_registration_success_since_startup' metric help
pub const SIGNER_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of successful signer registrations since startup on a Mithril signer node";

/// 'signer_registration_total_since_startup' metric name
pub const SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_signer_registration_total_since_startup";
/// 'signer_registration_total_since_startup' metric help
pub const SIGNER_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of signer registrations since startup on a Mithril signer node";

/// 'signer_registration_success_last_epoch' metric name
pub const SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME: &str =
    "mithril_signer_signer_registration_success_last_epoch";
/// 'signer_registration_success_last_epoch' metric help
pub const SIGNER_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP: &str =
    "Latest epoch at which signer successfully registered on a Mithril signer node";

/// 'signature_registration_success_since_startup' metric name
pub const SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_signature_registration_success_since_startup";
/// 'signature_registration_success_since_startup' metric help
pub const SIGNATURE_REGISTRATION_SUCCESS_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of successful signature registrations since startup on a Mithril signer node";

/// 'signature_registration_total_since_startup' metric name
pub const SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_signature_registration_total_since_startup";
/// 'signature_registration_total_since_startup' metric help
pub const SIGNATURE_REGISTRATION_TOTAL_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of signature registrations since startup on a Mithril signer node";

/// 'signature_registration_success_last_epoch' metric name
pub const SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_NAME: &str =
    "mithril_signer_signature_registration_success_last_epoch";
/// 'signature_registration_success_last_epoch' metric help
pub const SIGNATURE_REGISTRATION_SUCCESS_LAST_EPOCH_METRIC_HELP: &str =
    "Latest epoch at which signature successfully registered on a Mithril signer node";

/// 'runtime_cycle_success_since_startup' metric name
pub const RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_runtime_cycle_success_since_startup";
/// 'runtime_cycle_success_since_startup' metric help
pub const RUNTIME_CYCLE_SUCCESS_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of successful runtime cycles since startup on a Mithril signer node";

/// 'runtime_cycle_total_since_startup' metric name
pub const RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_NAME: &str =
    "mithril_signer_runtime_cycle_total_since_startup";
/// 'runtime_cycle_total_since_startup' metric help
pub const RUNTIME_CYCLE_TOTAL_SINCE_STARTUP_METRIC_HELP: &str =
    "Number of runtime cycles since startup on a Mithril signer node";
