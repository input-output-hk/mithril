use mithril_metric::{build_metrics_service, MetricsServiceExporter};

use mithril_metric::metric::{MetricCollector, MetricCounter};

build_metrics_service!(
    MetricsService,
    certificate_detail_total_served_since_startup:MetricCounter(
        "mithril_aggregator_certificate_detail_total_served_since_startup_counter",
        "Number of certificate details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_db_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup_counter",
        "Number of Cardano db artifact details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_mithril_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup_counter",
        "Number of Mithril stake distribution artifact details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_stake_distribution_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup_counter",
        "Number of Cardano stake distribution artifact details served since startup on a Mithril aggregator node"
    ),
    artifact_detail_cardano_transaction_total_served_since_startup:MetricCounter(
        "mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup_counter",
        "Number of Cardano transaction artifact details served since startup on a Mithril aggregator node"
    ),
    proof_cardano_transaction_total_proofs_served_since_startup:MetricCounter(
        "mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup_counter",
        "Number of Cardano transaction proofs served since startup on a Mithril aggregator node"
    ),
    proof_cardano_transaction_total_transactions_served_since_startup:MetricCounter(
        "mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup_counter",
        "Number of Cardano transaction hashes requested for proof since startup on a Mithril aggregator node"
    ),
    signer_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signer_registration_total_received_since_startup_counter",
        "Number of signer registrations received since startup on a Mithril aggregator node"
    ),
    signature_registration_total_received_since_startup:MetricCounter(
        "mithril_aggregator_signature_registration_total_received_since_startup_counter",
        "Number of signature registrations received since startup on a Mithril aggregator node"
    ),
    certificate_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_certificate_total_produced_since_startup_counter",
        "Number of certificates produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_db_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_db_total_produced_since_startup_counter",
        "Number of Cardano db artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_mithril_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup_counter",
        "Number of Mithril stake distribution artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_stake_distribution_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup_counter",
        "Number of Cardano stake distribution artifacts produced since startup on a Mithril aggregator node"
    ),
    artifact_cardano_transaction_total_produced_since_startup:MetricCounter(
        "mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup_counter",
        "Number of Cardano transaction artifacts produced since startup on a Mithril aggregator node"
    ),
    runtime_cycle_success_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_success_since_startup_counter",
        "Number of successful runtime cycles since startup on a Mithril aggregator"
    ),
    runtime_cycle_total_since_startup:MetricCounter(
        "mithril_aggregator_runtime_cycle_total_since_startup_counter",
        "Number of runtime cycles since startup on a Mithril aggregator"
    )

);
