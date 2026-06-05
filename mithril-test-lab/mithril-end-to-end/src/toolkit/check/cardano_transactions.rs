use anyhow::{Context, anyhow};
use slog_scope::info;
use std::time::Duration;

use mithril_common::{
    StdResult,
    entities::{Epoch, TransactionHash},
    messages::{CardanoTransactionSnapshotListMessage, CardanoTransactionSnapshotMessage},
};

use crate::{
    Aggregator, CardanoTransactionCommand, Client, ClientCommand, attempt,
    toolkit::{ScenarioToolkitContext, check::get_json_response},
    utils::AttemptResult,
};

#[derive(Debug, Clone, Default)]
pub struct CheckCardanoTransactionsToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoTransactionsToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn node_producing_cardano_transactions(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<String> {
        let url = format!("{}/artifact/cardano-transactions", aggregator.endpoint());
        info!("Waiting for the aggregator to produce a Cardano transactions artifact"; "aggregator" => &aggregator.name(), "aggregator" => &aggregator.name());

        async fn fetch_last_cardano_transaction_snapshot_hash(
            url: String,
        ) -> StdResult<Option<String>> {
            match get_json_response::<CardanoTransactionSnapshotListMessage>(url)
                .await?
                .as_deref()
            {
                Ok([artifact, ..]) => Ok(Some(artifact.hash.clone())),
                Ok(&[]) => Ok(None),
                Err(err) => Err(anyhow!("Invalid Cardano transactions artifact body: {err}",)),
            }
        }

        match attempt!(30, Duration::from_millis(2000), {
        fetch_last_cardano_transaction_snapshot_hash(url.clone()).await
    }) {
            AttemptResult::Ok(hash) => {
                info!("Aggregator produced a Cardano transactions artifact"; "hash" => &hash, "aggregator" => &aggregator.name());
                Ok(hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_transactions, no response from `{url}`"
        )),
        }
            .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
    }

    pub async fn signer_is_signing_cardano_transactions(
        &self,
        aggregator: &Aggregator,
        hash: &str,
        expected_epoch_min: Epoch,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/cardano-transaction/{hash}",
            aggregator.endpoint()
        );
        info!(
            "Asserting the aggregator is signing the Cardano transactions artifact `{}` with an expected min epoch of `{}`",
            hash,
            expected_epoch_min;
            "aggregator" => &aggregator.name()
        );

        async fn fetch_cardano_transaction_snapshot_message(
            url: String,
            expected_epoch_min: Epoch,
        ) -> StdResult<Option<CardanoTransactionSnapshotMessage>> {
            match get_json_response::<CardanoTransactionSnapshotMessage>(url).await? {
                Ok(artifact) => match artifact.epoch {
                    epoch if epoch >= expected_epoch_min => Ok(Some(artifact)),
                    epoch => Err(anyhow!(
                        "Minimum expected artifact epoch not reached: {epoch} < {expected_epoch_min}"
                    )),
                },
                Err(err) => Err(anyhow!(err).context("Invalid Cardano transactions artifact body")),
            }
        }

        match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_transaction_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
            AttemptResult::Ok(artifact) => {
                info!("Signer signed a Cardano transactions artifact"; "certificate_hash" => &artifact.certificate_hash, "aggregator" => &aggregator.name());
                Ok(artifact.certificate_hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_cardano_transactions, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn client_can_verify_transactions(
        &self,
        client: &mut Client,
        tx_hashes: Vec<TransactionHash>,
    ) -> StdResult<()> {
        #[allow(dead_code)]
        #[derive(Debug, serde::Deserialize)]
        struct ClientCardanoTransactionCertifyResult {
            certified_transactions: Vec<TransactionHash>,
            non_certified_transactions: Vec<TransactionHash>,
        }

        let result_file = client
            .run(ClientCommand::CardanoTransaction(
                CardanoTransactionCommand::Certify {
                    tx_hashes: tx_hashes.clone(),
                },
            ))
            .await?;
        info!("Client verified the Cardano transactions"; "tx_hashes" => ?tx_hashes);

        let file = std::fs::read_to_string(&result_file).with_context(|| {
            format!(
                "Failed to read client output from file `{}`",
                result_file.display()
            )
        })?;
        let result: ClientCardanoTransactionCertifyResult = serde_json::from_str(&file)
            .with_context(|| {
                format!(
                    "Failed to parse client output as json from file `{}`",
                    result_file.display()
                )
            })?;

        info!("Asserting that all Cardano transactions were verified by the Client...");
        if tx_hashes.iter().all(|tx| result.certified_transactions.contains(tx)) {
            Ok(())
        } else {
            Err(anyhow!(
                "Not all transactions were certified:\n'{:#?}'",
                result,
            ))
        }
    }
}
