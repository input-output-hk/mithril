use std::path::PathBuf;

use crate::{Aggregator, Devnet};
use mithril_common::StdResult;
use mithril_common::entities::{Epoch, ProtocolParameters};
use slog_scope::info;

pub async fn bootstrap_genesis_certificate(aggregator: &Aggregator) -> StdResult<()> {
    info!("Bootstrap genesis certificate"; "aggregator" => &aggregator.name());

    // A follower aggregator needs to wait few cycles of the state machine to be able to bootstrap
    // This should be removed when the aggregator is able to synchronize its certificate chain from another aggregator
    if !aggregator.is_first() {
        const CYCLES_WAIT_FOLLOWER: u64 = 3;
        tokio::time::sleep(std::time::Duration::from_millis(
            CYCLES_WAIT_FOLLOWER * aggregator.mithril_run_interval() as u64,
        ))
        .await;
    }

    info!("> stopping aggregator"; "aggregator" => &aggregator.name());
    aggregator.stop().await?;
    info!("> bootstrapping genesis using signers registered two epochs ago..."; "aggregator" => &aggregator.name());
    aggregator.bootstrap_genesis().await?;
    info!("> done, restarting aggregator"; "aggregator" => &aggregator.name());
    aggregator.serve().await?;

    Ok(())
}

pub async fn register_era_marker(
    aggregator: &Aggregator,
    devnet: &Devnet,
    mithril_era: &str,
    era_epoch: Epoch,
) -> StdResult<()> {
    info!("Register '{mithril_era}' era marker"; "aggregator" => &aggregator.name());

    info!("> generating era marker tx datum..."; "aggregator" => &aggregator.name());
    let tx_datum_file_path = devnet
        .artifacts_dir()
        .join(PathBuf::from("era-tx-datum.txt".to_string()));
    aggregator
        .era_generate_tx_datum(&tx_datum_file_path, mithril_era, era_epoch)
        .await?;

    info!("> writing '{mithril_era}' era marker on the Cardano chain..."; "aggregator" => &aggregator.name());
    devnet.write_era_marker(&tx_datum_file_path).await?;

    Ok(())
}

pub async fn delegate_stakes_to_pools(devnet: &Devnet, delegation_round: u16) -> StdResult<()> {
    info!("Delegate stakes to the cardano pools");

    devnet.delegate_stakes(delegation_round).await?;

    Ok(())
}

pub async fn transfer_funds(devnet: &Devnet) -> StdResult<()> {
    info!("Transfer funds on the devnet");

    devnet.transfer_funds().await?;

    Ok(())
}

pub async fn update_protocol_parameters(aggregator: &Aggregator) -> StdResult<()> {
    info!("Update protocol parameters"; "aggregator" => &aggregator.name());

    info!("> stopping aggregator");
    aggregator.stop().await?;
    let protocol_parameters_new = ProtocolParameters {
        k: 145,
        m: 210,
        phi_f: 0.80,
    };
    info!(
        "> updating protocol parameters to {protocol_parameters_new:?}..."; "aggregator" => &aggregator.name()
    );
    aggregator
        .set_protocol_parameters(&protocol_parameters_new)
        .await;
    info!("> done, restarting aggregator"; "aggregator" => &aggregator.name());
    aggregator.serve().await?;

    Ok(())
}
