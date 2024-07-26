use clap::Parser;
use slog_scope::info;
use std::{ops::Deref, sync::Arc, time::Duration};
use tokio::sync::oneshot;

use mithril_common::{
    digesters::{DummyImmutableDb, DummyImmutablesDbBuilder},
    entities::{CardanoDbBeacon, Epoch, ProtocolParameters, SignedEntityType, SingleSignatures},
    test_utils::MithrilFixture,
    StdResult,
};

use mithril_end_to_end::{
    stress_test::{
        aggregator_helpers, entities::*, fake_chain, fake_client::clients_scenario, fake_signer,
        payload_builder, wait,
    },
    Aggregator,
};

fn init_logger(opts: &MainOpts) -> slog_scope::GlobalLoggerGuard {
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let drain = slog::LevelFilter::new(drain, opts.log_level()).fuse();

    slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()))
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let opts = MainOpts::parse();
    let mut reporter: Reporter = Reporter::new(opts.num_signers, opts.num_clients);
    reporter.start("stress tests bootstrap");
    // configure a dummy immutable db
    let immutable_db = DummyImmutablesDbBuilder::new("load-tester")
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();

    let _logger_guard = init_logger(&opts);
    let aggregator_parameters = AggregatorParameters::new(&opts, &immutable_db.dir)?;
    let mut current_epoch = Epoch(1);
    let protocol_parameters = ProtocolParameters::new(2422, 20973, 0.20);
    info!(">> Starting stress test with options: {opts:?}");

    reporter.start("stress bootstrap");
    info!(">> Creation of the Signer Key Registrations payloads");
    let signers_fixture =
        payload_builder::generate_signer_data(opts.num_signers, protocol_parameters);

    let mithril_stake_distribution_signatures =
        payload_builder::precompute_mithril_stake_distribution_signatures(
            &signers_fixture,
            Duration::from_secs(180),
        )
        .await?;

    let aggregator = aggregator_helpers::bootstrap_aggregator(
        &aggregator_parameters,
        &signers_fixture,
        &mut current_epoch,
    )
    .await?;
    let aggregator_endpoint = aggregator.endpoint();
    reporter.stop();

    let mut scenario_parameters = ScenarioParameters {
        aggregator,
        aggregator_parameters,
        signers_fixture,
        immutable_db,
        reporter,
        precomputed_mithril_stake_distribution_signatures: mithril_stake_distribution_signatures,
    };

    let scenario_counters = ScenarioCounters {
        current_epoch,
        number_of_certificates: 1,
        number_of_mithril_stake_distributions: 0,
        number_of_snapshots: 0,
    };

    info!(">> Run phase 1 without client load");
    let scenario_counters = main_scenario(scenario_counters, &mut scenario_parameters).await?;

    info!(">> Run phase 2 with client load");
    let (shutdown_tx, mut shutdown_rx) = oneshot::channel();
    let clients_handle = tokio::spawn(async move {
        if opts.num_clients > 0 {
            loop {
                tokio::select! {
                    _msg = &mut shutdown_rx => {
                        break;
                    }
                    _ = clients_scenario(aggregator_endpoint.clone(), opts.num_clients) => {

                    }
                }
            }
        }
    });
    main_scenario(scenario_counters, &mut scenario_parameters)
        .await
        .expect("the main scenario should not fail");
    let _ = shutdown_tx.send(());
    clients_handle.await.unwrap();

    info!(">> Display execution timings:");
    scenario_parameters.reporter.print_report();

    info!(">> All steps executed successfully, stopping all tasks...");
    scenario_parameters.aggregator.stop().await.unwrap();

    Ok(())
}

struct ScenarioParameters {
    aggregator: Aggregator,
    aggregator_parameters: AggregatorParameters,
    signers_fixture: MithrilFixture,
    immutable_db: DummyImmutableDb,
    precomputed_mithril_stake_distribution_signatures: Vec<SingleSignatures>,
    reporter: Reporter,
}

#[derive(Clone, Copy)]
struct ScenarioCounters {
    current_epoch: Epoch,
    number_of_certificates: usize,
    number_of_mithril_stake_distributions: usize,
    number_of_snapshots: usize,
}

async fn main_scenario(
    counters: ScenarioCounters,
    parameters: &mut ScenarioParameters,
) -> StdResult<ScenarioCounters> {
    info!(">> Move one epoch forward in order to start creating certificates");
    let ScenarioCounters {
        mut current_epoch,
        mut number_of_certificates,
        mut number_of_mithril_stake_distributions,
        mut number_of_snapshots,
    } = counters;
    current_epoch += 1;

    fake_chain::set_epoch(
        &parameters.aggregator_parameters.mock_epoch_file_path(),
        current_epoch,
    );

    // Creating the new immutable file early will avoid time effects due to the aggregator runtime design when high client traffic is sent
    info!(">> Add new immutable file");
    parameters.immutable_db.add_immutable_file();

    wait::for_epoch_settings_at_epoch(
        &parameters.aggregator,
        Duration::from_secs(60),
        current_epoch,
    )
    .await?;

    info!(">> Send the Signer Key Registrations payloads");
    parameters.reporter.start("signers registration");
    fake_signer::try_register_signer_until_registration_round_is_open(
        &parameters.aggregator,
        &parameters.signers_fixture.signers()[0],
        current_epoch + 1,
        Duration::from_secs(60),
    )
    .await?;
    let errors = fake_signer::register_signers_to_aggregator(
        &parameters.aggregator,
        &parameters.signers_fixture.signers()[1..],
        current_epoch + 1,
    )
    .await?;
    parameters.reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for pending certificate to be available");
    wait::for_pending_certificate(
        &parameters.aggregator,
        Duration::from_secs(60),
        &SignedEntityType::MithrilStakeDistribution(current_epoch),
    )
    .await?;

    info!(
        ">> Send the Signer Signatures payloads for MithrilStakeDistribution({:?})",
        current_epoch
    );
    parameters.reporter.start("signatures registration");
    let errors = fake_signer::register_signatures_to_aggregator(
        &parameters.aggregator,
        &parameters.precomputed_mithril_stake_distribution_signatures,
        SignedEntityType::MithrilStakeDistribution(current_epoch),
    )
    .await?;
    parameters.reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for certificates to be available...");
    number_of_certificates += 1;
    wait::for_certificates(
        &parameters.aggregator,
        number_of_certificates,
        Duration::from_secs(120),
    )
    .await?;

    info!(">> Wait for artifacts to be available...");
    number_of_mithril_stake_distributions += 1;
    wait::for_mithril_stake_distribution_artifacts(
        &parameters.aggregator,
        number_of_mithril_stake_distributions,
        Duration::from_secs(60),
    )
    .await?;

    info!(">> Wait for pending certificate to be available");
    wait::for_pending_certificate(
        &parameters.aggregator,
        Duration::from_secs(60),
        &SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
            "devnet".to_string(),
            *current_epoch.deref(),
            parameters.immutable_db.last_immutable_number().unwrap() - 1,
        )),
    )
    .await?;

    info!(">> Compute the immutable files signature");
    let (current_beacon, immutable_files_signatures) =
        payload_builder::compute_immutable_files_signatures(
            &parameters.immutable_db,
            current_epoch,
            &parameters.signers_fixture,
            Duration::from_secs(60),
        )
        .await
        .unwrap();

    info!(
        ">> Send the Signer Signatures payloads for CardanoImmutableFiles({:?})",
        current_beacon
    );
    parameters.reporter.start("signatures registration");
    let errors = fake_signer::register_signatures_to_aggregator(
        &parameters.aggregator,
        &immutable_files_signatures,
        SignedEntityType::CardanoImmutableFilesFull(current_beacon),
    )
    .await?;
    parameters.reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for certificates to be available...");
    number_of_certificates += 1;
    wait::for_certificates(
        &parameters.aggregator,
        number_of_certificates,
        Duration::from_secs(120),
    )
    .await?;

    info!(">> Wait for artifacts to be available...");
    number_of_snapshots += 1;
    wait::for_immutable_files_artifacts(
        &parameters.aggregator,
        number_of_snapshots,
        Duration::from_secs(60),
    )
    .await?;

    Ok(ScenarioCounters {
        current_epoch,
        number_of_certificates,
        number_of_mithril_stake_distributions,
        number_of_snapshots,
    })
}
