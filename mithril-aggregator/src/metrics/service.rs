use mithril_metric::{build_metrics_service, MetricsServiceExporter};
use prometheus::Registry;
use slog::Logger;

use mithril_common::{entities::Epoch, StdResult};

use mithril_metric::metric::{CounterValue, MetricCollector, MetricCounter, MetricGauge};

build_metrics_service!(
    MetricsService,
    // signer_registration_success_since_startup_counter:MetricCounter(
    //     "mithril_signer_signer_registration_success_since_startup",
    //     "Number of successful signer registrations since startup on a Mithril signer node"
    // ),
    // signer_registration_total_since_startup_counter:MetricCounter(
    //     "mithril_signer_signer_registration_total_since_startup",
    //     "Number of signer registrations since startup on a Mithril signer node"
    // ),
    // signer_registration_success_last_epoch_gauge:MetricGauge(
    //     "mithril_signer_signer_registration_success_last_epoch",
    //     "Latest epoch at which signer successfully registered on a Mithril signer node"
    // ),
    // signature_registration_success_since_startup_counter:MetricCounter(
    //     "mithril_signer_signature_registration_success_since_startup",
    //     "Number of successful signature registrations since startup on a Mithril signer node"
    // ),
    // signature_registration_total_since_startup_counter:MetricCounter(
    //     "mithril_signer_signature_registration_total_since_startup",
    //     "Number of signature registrations since startup on a Mithril signer node"
    // ),
    // signature_registration_success_last_epoch_gauge:MetricGauge(
    //     "mithril_signer_signature_registration_success_last_epoch",
    //     "Latest epoch at which signature successfully registered on a Mithril signer node"
    // ),
    // // Runtime cycle metrics
    // runtime_cycle_success_since_startup_counter:MetricCounter(
    //     "mithril_signer_runtime_cycle_success_since_startup",
    //     "Number of successful runtime cycles since startup on a Mithril signer node"
    // ),
    // runtime_cycle_total_since_startup_counter:MetricCounter(
    //     "mithril_signer_runtime_cycle_total_since_startup",
    //     "Number of runtime cycles since startup on a Mithril signer node"
    // )

    certificate_detail_total_served_since_startup:MetricCounter(
        "mithril_aggregator_certificate_detail_total_served_since_startup_counter",
        "certificate_detail_total_served_since_startup"
    ),
    artifact_cardano_db_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_db_total_served_since_startup_counter",
        "artifact_cardano_db_total_served_since_startup"
    ),
    artifact_mithril_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_mithril_stake_distribution_total_served_since_startup_counter",
        "artifact_mithril_stake_distribution_total_served_since_startup"
    ),
    artifact_cardano_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_stake_distribution_total_served_since_startup_counter",
        "artifact_cardano_stake_distribution_total_served_since_startup"
    ),
    artifact_cardano_transaction_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_transaction_total_served_since_startup_counter",
        "artifact_cardano_transaction_total_served_since_startup"
    ),
    proof_cardano_transaction_total_served_since_startup:MetricCounter(
        "mithril_aggregator_proof_cardano_transaction_total_served_since_startup_counter",
        "proof_cardano_transaction_total_served_since_startup"
    ),
    signer_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signer_registration_total_received_since_startup_counter",
        "signer_registration_total_received_since_startup"
    ),
    signature_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signature_registration_total_received_since_startup_counter",
        "signature_registration_total_received_since_startup"
    ),
    certificate_detail_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_certificate_detail_total_produced_since_startup_counter",
        "certificate_detail_total_produced_since_startup"
    ),
    artifact_cardano_db_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_db_total_produced_since_startup_counter",
        "artifact_cardano_db_total_produced_since_startup"
    ),
    artifact_mithril_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup_counter",
        "artifact_mithril_stake_distribution_total_produced_since_startup"
    ),
    artifact_cardano_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup_counter",
        "artifact_cardano_stake_distribution_total_produced_since_startup"
    ),
    artifact_cardano_transaction_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup_counter",
        "artifact_cardano_transaction_total_produced_since_startup"
    ),
    runtime_cycle_success_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_success_since_startup_counter",
        "runtime_cycle_success_since_startup"
    ),
    runtime_cycle_total_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_total_since_startup_counter",
        "runtime_cycle_total_since_startup"
    )

);

#[cfg(test)]
mod tests {
    // TODO Do we need tests?
}
