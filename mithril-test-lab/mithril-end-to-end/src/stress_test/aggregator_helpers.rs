use slog_scope::info;
use std::time::Duration;

use mithril_common::{entities::Epoch, test_utils::MithrilFixture, StdResult};

use crate::{
    stress_test::{entities::AggregatorParameters, fake_chain, fake_signer, wait},
    Aggregator, AggregatorConfig,
};

/// Bootstrap an aggregator and make it compute its genesis certificate
pub async fn bootstrap_aggregator(
    args: &AggregatorParameters,
    signers_fixture: &MithrilFixture,
    current_epoch: &mut Epoch,
) -> StdResult<Aggregator> {
    info!(">> Launch Aggregator");
    let signed_entity_types = vec![];
    let chain_observer_type = "cardano-cli";

    let mut aggregator = Aggregator::new(&AggregatorConfig {
        server_port: args.server_port as u64,
        bft_node: &args.bft_node,
        cardano_cli_path: &args.cardano_cli_path,
        work_dir: &args.work_dir,
        bin_dir: &args.bin_dir,
        cardano_node_version: "1.2.3",
        mithril_run_interval: 1000,
        mithril_era: &args.mithril_era,
        mithril_era_marker_address: "",
        mithril_era_reader_adapter: "dummy",
        signed_entity_types: &signed_entity_types,
        chain_observer_type,
    })
    .unwrap();

    fake_chain::set_epoch(&args.mock_epoch_file_path(), *current_epoch);
    fake_chain::set_stake_distribution(&args.mock_stake_distribution_file_path(), signers_fixture);

    // Extremely large interval since, for the two following starts, only the http_server part
    // of the aggregator is relevant as we need to send signer registrations.
    aggregator.change_run_interval(Duration::from_secs(20000));
    aggregator.set_mock_cardano_cli_file_path(
        &args.mock_stake_distribution_file_path(),
        &args.mock_epoch_file_path(),
    );
    aggregator.set_protocol_parameters(&signers_fixture.protocol_parameters());

    info!(
        ">> Starting the aggregator with a large run interval to call the http_server\
    without being bothered by the state machine cycles"
    );
    aggregator.serve().unwrap();
    wait::for_http_response(
        &format!("{}/epoch-settings", aggregator.endpoint()),
        Duration::from_secs(10),
        "Waiting for the aggregator to start",
    )
    .await?;

    info!(">> Send the Signer Key Registrations payloads for the genesis signers");
    let errors = fake_signer::register_signers_to_aggregator(
        &aggregator,
        &signers_fixture.signers(),
        *current_epoch + 1,
    )
    .await?;
    assert_eq!(0, errors);
    aggregator.stop().await.unwrap();

    info!(">> Move one epoch forward in order to issue the genesis certificate");
    *current_epoch += 1;
    fake_chain::set_epoch(&args.mock_epoch_file_path(), *current_epoch);

    info!(">> Restarting the aggregator still with a large run interval");
    aggregator.serve().unwrap();
    wait::for_http_response(
        &format!("{}/epoch-settings", aggregator.endpoint()),
        Duration::from_secs(10),
        "Waiting for the aggregator to start",
    )
    .await?;

    info!(">> Send the Signer Key Registrations payloads for next genesis signers");
    let errors = fake_signer::register_signers_to_aggregator(
        &aggregator,
        &signers_fixture.signers(),
        *current_epoch + 1,
    )
    .await?;
    assert_eq!(0, errors);
    aggregator.stop().await.unwrap();

    {
        info!(">> Compute genesis certificate");
        let mut genesis_aggregator = Aggregator::copy_configuration(&aggregator);
        genesis_aggregator
            .bootstrap_genesis()
            .await
            .expect("Genesis aggregator should be able to bootstrap genesis");
    }

    info!(">> Restart aggregator with a normal run interval");
    aggregator.change_run_interval(Duration::from_secs(3));
    aggregator.serve().unwrap();

    wait::for_http_response(
        &format!("{}/epoch-settings", aggregator.endpoint()),
        Duration::from_secs(10),
        "Waiting for the aggregator to restart",
    )
    .await?;

    info!(">> Aggregator bootrapped");

    Ok(aggregator)
}
