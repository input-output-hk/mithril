use mithril_metric::{build_metrics_service, MetricsServiceExporter};

use mithril_metric::metric::{MetricCollector, MetricCounter, MetricGauge};

build_metrics_service!(
    MetricsService,
    signer_registration_success_since_startup_counter:MetricCounter(
        "mithril_signer_signer_registration_success_since_startup",
        "Number of successful signer registrations since startup on a Mithril signer node"
    ),
    signer_registration_total_since_startup_counter:MetricCounter(
        "mithril_signer_signer_registration_total_since_startup",
        "Number of signer registrations since startup on a Mithril signer node"
    ),
    signer_registration_success_last_epoch_gauge:MetricGauge(
        "mithril_signer_signer_registration_success_last_epoch",
        "Latest epoch at which signer successfully registered on a Mithril signer node"
    ),
    signature_registration_success_since_startup_counter:MetricCounter(
        "mithril_signer_signature_registration_success_since_startup",
        "Number of successful signature registrations since startup on a Mithril signer node"
    ),
    signature_registration_total_since_startup_counter:MetricCounter(
        "mithril_signer_signature_registration_total_since_startup",
        "Number of signature registrations since startup on a Mithril signer node"
    ),
    signature_registration_success_last_epoch_gauge:MetricGauge(
        "mithril_signer_signature_registration_success_last_epoch",
        "Latest epoch at which signature successfully registered on a Mithril signer node"
    ),
    runtime_cycle_success_since_startup_counter:MetricCounter(
        "mithril_signer_runtime_cycle_success_since_startup",
        "Number of successful runtime cycles since startup on a Mithril signer node"
    ),
    runtime_cycle_total_since_startup_counter:MetricCounter(
        "mithril_signer_runtime_cycle_total_since_startup",
        "Number of runtime cycles since startup on a Mithril signer node"
    )

);
