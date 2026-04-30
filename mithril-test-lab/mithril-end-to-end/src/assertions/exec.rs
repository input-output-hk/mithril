use std::path::PathBuf;

use crate::{Aggregator, Devnet};
use anyhow::Context;
use mithril_common::StdResult;
use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_common::messages::AggregatorStatusMessage;
use slog_scope::info;

/// Retrieve the current Mithril era from a running aggregator by querying its `/status` route.
pub async fn retrieve_current_era(aggregator: &Aggregator) -> StdResult<String> {
    let url = format!("{}/status", aggregator.endpoint());
    let response = reqwest::get(&url)
        .await
        .with_context(|| format!("Failed to query aggregator status at `{url}`"))?;
    let status_message: AggregatorStatusMessage = response
        .json()
        .await
        .with_context(|| "Failed to parse aggregator status response")?;

    Ok(status_message.mithril_era.to_string())
}

pub async fn bootstrap_genesis_certificate(aggregator: &Aggregator) -> StdResult<()> {
    info!("Bootstrap genesis certificate"; "aggregator" => &aggregator.name());
    info!("> retrieving current era from aggregator"; "aggregator" => &aggregator.name());
    let mithril_era = retrieve_current_era(aggregator).await?;
    info!("> stopping aggregator"; "aggregator" => &aggregator.name());
    aggregator.stop().await?;
    info!("> bootstrapping genesis using signers registered two epochs ago..."; "aggregator" => &aggregator.name());
    aggregator.bootstrap_genesis(&mithril_era).await?;
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
    let protocol_parameters_new = if aggregator.aggregate_signature_type == "Concatenation" {
        ProtocolParameters {
            k: 145,
            m: 210,
            phi_f: 0.80,
        }
    } else {
        ProtocolParameters {
            k: 7,
            m: 10,
            phi_f: 0.95,
        }
    };

    info!(
        "> updating protocol parameters to {protocol_parameters_new:?}..."; "aggregator" => &aggregator.name()
    );
    aggregator.set_protocol_parameters(&protocol_parameters_new).await;
    info!("> done, restarting aggregator"; "aggregator" => &aggregator.name());
    aggregator.serve().await?;

    Ok(())
}
