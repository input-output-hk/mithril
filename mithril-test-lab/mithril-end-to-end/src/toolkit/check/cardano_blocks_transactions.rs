use anyhow::{Context, anyhow};
use slog_scope::{info, warn};
use std::time::Duration;

use mithril_common::{
    StdResult,
    entities::{BlockHash, Epoch, TransactionHash},
    messages::{
        CardanoBlocksTransactionsSnapshotListMessage, CardanoBlocksTransactionsSnapshotMessage,
    },
};

use crate::{
    Aggregator, CardanoBlockCommand, CardanoTransactionV2Command, Client, ClientCommand, attempt,
    toolkit::{ScenarioToolkitContext, check::get_json_response},
    utils::AttemptResult,
};

#[derive(Debug, Clone, Default)]
pub struct CheckCardanoBlocksTransactionsToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoBlocksTransactionsToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn node_producing_cardano_blocks_transactions(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/cardano-blocks-transactions",
            aggregator.endpoint()
        );
        info!("Waiting for the aggregator to produce a Cardano blocks transactions artifact"; "aggregator" => &aggregator.name());

        async fn fetch_last_cardano_blocks_transactions_snapshot_hash(
            url: String,
        ) -> StdResult<Option<String>> {
            match get_json_response::<CardanoBlocksTransactionsSnapshotListMessage>(url)
                .await?
                .as_deref()
            {
                Ok([artifact, ..]) => Ok(Some(artifact.hash.clone())),
                Ok(&[]) => Ok(None),
                Err(err) => Err(anyhow!(
                    "Invalid Cardano blocks transactions artifact body: {err}",
                )),
            }
        }

        match attempt!(30, Duration::from_millis(2000), {
        fetch_last_cardano_blocks_transactions_snapshot_hash(url.clone()).await
    }) {
            AttemptResult::Ok(hash) => {
                info!("Aggregator produced a Cardano blocks transactions artifact"; "hash" => &hash, "aggregator" => &aggregator.name());
                Ok(hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_blocks_transactions, no response from `{url}`"
        )),
        }
            .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
    }

    pub async fn signer_is_signing_cardano_blocks_transactions(
        &self,
        aggregator: &Aggregator,
        hash: &str,
        expected_epoch_min: Epoch,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/cardano-blocks-transactions/{hash}",
            aggregator.endpoint()
        );
        info!(
            "Asserting the aggregator is signing the Cardano blocks transactions artifact `{}` with an expected min epoch of `{}`",
            hash,
            expected_epoch_min;
            "aggregator" => &aggregator.name()
        );

        async fn fetch_cardano_blocks_transactions_snapshot_message(
            url: String,
            expected_epoch_min: Epoch,
        ) -> StdResult<Option<CardanoBlocksTransactionsSnapshotMessage>> {
            match get_json_response::<CardanoBlocksTransactionsSnapshotMessage>(url).await? {
                Ok(artifact) => match artifact.epoch {
                    epoch if epoch >= expected_epoch_min => Ok(Some(artifact)),
                    epoch => Err(anyhow!(
                        "Minimum expected artifact epoch not reached: {epoch} < {expected_epoch_min}"
                    )),
                },
                Err(err) => {
                    Err(anyhow!(err).context("Invalid Cardano blocks transactions artifact body"))
                }
            }
        }

        match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_blocks_transactions_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
            AttemptResult::Ok(artifact) => {
                info!("Signer signed a Cardano blocks transactions artifact"; "certificate_hash" => &artifact.certificate_hash, "aggregator" => &aggregator.name());
                Ok(artifact.certificate_hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_cardano_blocks_transactions, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn client_verify_transactions(
        &self,
        client: &mut Client,
        tx_hashes: Vec<TransactionHash>,
    ) -> StdResult<()> {
        #[allow(dead_code)]
        #[derive(Debug, serde::Deserialize)]
        struct ClientCardanoTransactionCertifyResult {
            certified_transactions: Vec<CertifiedTransactionV2>,
            non_certified_transactions: Vec<TransactionHash>,
        }

        #[derive(Debug, serde::Deserialize)]
        struct CertifiedTransactionV2 {
            transaction_hash: String,
        }

        if !client.version().is_above_or_equal("0.13.1") {
            warn!(
                "Client version is below 0.13.1, skipping `cardano-transaction certify --backend v2` check"
            );
            return Ok(());
        }

        let result_file = client
            .run(ClientCommand::CardanoTransactionV2(
                CardanoTransactionV2Command::Certify {
                    tx_hashes: tx_hashes.clone(),
                },
            ))
            .await?;
        info!("Client verified the Cardano transactions V2"; "tx_hashes" => ?tx_hashes);

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

        info!("Asserting that all Cardano transactions V2 were verified by the Client...");
        let certified_tx_hashes_result: Vec<String> = result
            .certified_transactions
            .iter()
            .map(|tx| tx.transaction_hash.clone())
            .collect();

        if tx_hashes.iter().all(|tx| certified_tx_hashes_result.contains(tx)) {
            Ok(())
        } else {
            Err(anyhow!(
                "Not all transactions V2 were certified:\n'{:#?}'",
                result,
            ))
        }
    }

    pub async fn client_verify_blocks(
        &self,
        client: &mut Client,
        block_hashes: Vec<BlockHash>,
    ) -> StdResult<()> {
        #[allow(dead_code)]
        #[derive(Debug, serde::Deserialize)]
        struct ClientCardanoBlockCertifyResult {
            certified_blocks: Vec<CertifiedBlock>,
            non_certified_blocks: Vec<BlockHash>,
        }

        #[derive(Debug, serde::Deserialize)]
        struct CertifiedBlock {
            block_hash: String,
        }

        if !client.version().is_above_or_equal("0.13.1") {
            warn!("Client version is below 0.13.1, skipping `cardano-block certify` check");
            return Ok(());
        }

        let result_file = client
            .run(ClientCommand::CardanoBlock(CardanoBlockCommand::Certify {
                block_hashes: block_hashes.clone(),
            }))
            .await?;
        info!("Client verified the Cardano blocks"; "block_hashes" => ?block_hashes);
        let file = std::fs::read_to_string(&result_file).with_context(|| {
            format!(
                "Failed to read client output from file `{}`",
                result_file.display()
            )
        })?;
        let result: ClientCardanoBlockCertifyResult =
            serde_json::from_str(&file).with_context(|| {
                format!(
                    "Failed to parse client output as json from file `{}`",
                    result_file.display()
                )
            })?;

        info!("Asserting that all Cardano blocks were verified by the Client...");
        let certified_blocks_hashes_result: Vec<String> = result
            .certified_blocks
            .iter()
            .map(|block| block.block_hash.clone())
            .collect();

        if block_hashes
            .iter()
            .all(|block| certified_blocks_hashes_result.contains(block))
        {
            Ok(())
        } else {
            Err(anyhow!("Not all blocks were certified:\n'{:#?}'", result,))
        }
    }
}
