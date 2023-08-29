use clap::Parser;
use slog_scope::info;
use std::{sync::Arc, time::Duration};

use mithril_common::{
    digesters::DummyImmutablesDbBuilder,
    entities::{Epoch, ProtocolParameters, SignedEntityType},
    StdResult,
};

use mithril_end_to_end::stress_test::{
    aggregator_helpers, entities::*, fake_chain, fake_signer, payload_builder, wait,
};

fn init_logger(opts: &MainOpts) -> slog_scope::GlobalLoggerGuard {
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let drain = slog::LevelFilter::new(drain, opts.log_level()).fuse();

    slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()))
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> StdResult<()> {
    let opts = MainOpts::parse();
    let mut reporter: Reporter = Reporter::new(opts.num_signers);
    reporter.start("stress tests bootstrap");
    // configure a dummy immutable db
    let mut immutable_db = DummyImmutablesDbBuilder::new("load-tester")
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();

    let _logger_guard = init_logger(&opts);
    let args = AggregatorParameters::new(&opts, &immutable_db.dir)?;
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

    let mut aggregator =
        aggregator_helpers::bootstrap_aggregator(&args, &signers_fixture, &mut current_epoch)
            .await?;
    reporter.stop();

    info!(">> Move one epoch forward in order to start creating certificates");
    current_epoch += 1;
    fake_chain::set_epoch(&args.mock_epoch_file_path(), current_epoch);
    wait::for_epoch_settings_at_epoch(&aggregator, Duration::from_secs(10), current_epoch).await?;

    info!(">> Send the Signer Key Registrations payloads");
    reporter.start("signers registration");
    let errors = fake_signer::register_signers_to_aggregator(
        &aggregator,
        &signers_fixture.signers(),
        current_epoch + 1,
    )
    .await?;
    reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for pending certificate to be available");
    wait::for_pending_certificate(&aggregator, Duration::from_secs(30)).await?;

    info!(
        ">> Send the Signer Signatures payloads for MithrilStakeDistribution({:?})",
        current_epoch
    );
    reporter.start("signatures registration");
    let errors = fake_signer::register_signatures_to_aggregator(
        &aggregator,
        &mithril_stake_distribution_signatures,
        SignedEntityType::MithrilStakeDistribution(current_epoch),
    )
    .await?;
    reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for certificates to be available...");
    wait::for_certificates(&aggregator, 1, Duration::from_secs(30)).await?;

    info!(">> Wait for artifacts to be available...");
    wait::for_mithril_stake_distribution_artifacts(&aggregator, Duration::from_secs(30)).await?;

    info!(">> Add new immutable file");
    immutable_db.add_immutable_file();

    info!(">> Wait for pending certificate to be available");
    wait::for_pending_certificate(&aggregator, Duration::from_secs(30)).await?;

    info!(">> Compute the immutable files signature");
    let (current_beacon, immutable_files_signatures) =
        payload_builder::compute_immutable_files_signatures(
            &immutable_db,
            current_epoch,
            &signers_fixture,
            Duration::from_secs(30),
        )
        .await
        .unwrap();

    info!(
        ">> Send the Signer Signatures payloads for CardanoImmutableFiles({:?})",
        current_beacon
    );
    reporter.start("signatures registration");
    let errors = fake_signer::register_signatures_to_aggregator(
        &aggregator,
        &immutable_files_signatures,
        SignedEntityType::CardanoImmutableFilesFull(current_beacon),
    )
    .await?;
    reporter.stop();
    assert_eq!(0, errors);

    info!(">> Wait for certificates to be available...");
    wait::for_certificates(&aggregator, 2, Duration::from_secs(30)).await?;

    info!(">> Wait for artifacts to be available...");
    wait::for_immutable_files_artifacts(&aggregator, Duration::from_secs(30)).await?;

    info!(">> Display execution timings:");
    reporter.print_report();

    info!(">> All steps executed successfully, stopping all tasks...");
    aggregator.stop().await.unwrap();

    Ok(())
}
