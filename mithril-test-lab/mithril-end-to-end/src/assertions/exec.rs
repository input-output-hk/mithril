use std::path::PathBuf;

use crate::{Aggregator, Devnet};
use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_common::StdResult;
use slog_scope::info;

pub async fn bootstrap_genesis_certificate(aggregator: &mut Aggregator) -> StdResult<()> {
    info!("Bootstrap genesis certificate");

    info!("> stopping aggregator");
    aggregator.stop().await?;
    info!("> bootstrapping genesis using signers registered two epochs ago...");
    aggregator.bootstrap_genesis().await?;
    info!("> done, restarting aggregator");
    aggregator.serve()?;

    Ok(())
}

pub async fn register_era_marker(
    aggregator: &mut Aggregator,
    devnet: &Devnet,
    mithril_era: &str,
    era_epoch: Epoch,
) -> StdResult<()> {
    info!("Register '{mithril_era}' era marker");

    info!("> generating era marker tx datum...");
    let tx_datum_file_path = devnet
        .artifacts_dir()
        .join(PathBuf::from("era-tx-datum.txt".to_string()));
    aggregator
        .era_generate_tx_datum(&tx_datum_file_path, mithril_era, era_epoch)
        .await?;

    info!("> writing '{mithril_era}' era marker on the Cardano chain...");
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

pub async fn update_protocol_parameters(aggregator: &mut Aggregator) -> StdResult<()> {
    info!("Update protocol parameters");

    info!("> stopping aggregator");
    aggregator.stop().await?;
    let protocol_parameters_new = ProtocolParameters {
        k: 145,
        m: 210,
        phi_f: 0.80,
    };
    info!(
        "> updating protocol parameters to {:?}...",
        protocol_parameters_new
    );
    aggregator.set_protocol_parameters(&protocol_parameters_new);
    info!("> done, restarting aggregator");
    aggregator.serve()?;

    Ok(())
}
