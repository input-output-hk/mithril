use crate::{
    attempt, utils::AttemptResult, CardanoDbCommand, CardanoStakeDistributionCommand,
    CardanoTransactionCommand, Client, ClientCommand, MithrilStakeDistributionCommand,
};
use anyhow::{anyhow, Context};
use mithril_common::{
    entities::{Epoch, TransactionHash},
    messages::{
        CardanoDatabaseSnapshotListMessage, CardanoDatabaseSnapshotMessage,
        CardanoStakeDistributionListMessage, CardanoStakeDistributionMessage,
        CardanoTransactionSnapshotListMessage, CardanoTransactionSnapshotMessage,
        CertificateMessage, MithrilStakeDistributionListMessage, MithrilStakeDistributionMessage,
        SnapshotMessage,
    },
    StdResult,
};
use reqwest::StatusCode;
use slog_scope::info;
use std::time::Duration;

pub async fn assert_node_producing_mithril_stake_distribution(
    aggregator_endpoint: &str,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/mithril-stake-distributions");
    info!("Waiting for the aggregator to produce a mithril stake distribution");

    async fn fetch_last_mithril_stake_distribution_hash(url: String) -> StdResult<Option<String>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response
                    .json::<MithrilStakeDistributionListMessage>()
                    .await
                    .as_deref()
                {
                    Ok([stake_distribution, ..]) => Ok(Some(stake_distribution.hash.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(anyhow!("Invalid mithril stake distribution body : {err}",)),
                },
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        fetch_last_mithril_stake_distribution_hash(url.clone()).await
    }) {
        AttemptResult::Ok(hash) => {
            info!("Aggregator produced a mithril stake distribution"; "hash" => &hash);
            Ok(hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_mithril_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_mithril_stake_distribution(
    aggregator_endpoint: &str,
    hash: &str,
    expected_epoch_min: Epoch,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/mithril-stake-distribution/{hash}");
    info!(
        "Asserting the aggregator is signing the mithril stake distribution message `{}` with an expected min epoch of `{}`",
        hash,
        expected_epoch_min
    );

    async fn fetch_mithril_stake_distribution_message(
        url: String,
        expected_epoch_min: Epoch,
    ) -> StdResult<Option<MithrilStakeDistributionMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<MithrilStakeDistributionMessage>().await {
                    Ok(stake_distribution) => match stake_distribution.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(stake_distribution)),
                        epoch => Err(anyhow!(
                            "Minimum expected mithril stake distribution epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(anyhow!(err).context("Invalid mithril stake distribution body",)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_mithril_stake_distribution_message(url.clone(), expected_epoch_min).await
    }) {
        AttemptResult::Ok(stake_distribution) => {
            // todo: assert that the mithril stake distribution is really signed
            info!("Signer signed a mithril stake distribution"; "certificate_hash" => &stake_distribution.certificate_hash);
            Ok(stake_distribution.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_mithril_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_node_producing_snapshot(aggregator_endpoint: &str) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/snapshots");
    info!("Waiting for the aggregator to produce a snapshot");

    async fn fetch_last_snapshot_digest(url: String) -> StdResult<Option<String>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<SnapshotMessage>>().await.as_deref() {
                    Ok([snapshot, ..]) => Ok(Some(snapshot.digest.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(anyhow!("Invalid snapshot body : {err}",)),
                },
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        fetch_last_snapshot_digest(url.clone()).await
    }) {
        AttemptResult::Ok(digest) => {
            info!("Aggregator produced a snapshot"; "digest" => &digest);
            Ok(digest)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_snapshot(
    aggregator_endpoint: &str,
    digest: &str,
    expected_epoch_min: Epoch,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/snapshot/{digest}");
    info!(
        "Asserting the aggregator is signing the snapshot message `{}` with an expected min epoch of `{}`",
        digest,
        expected_epoch_min
    );

    async fn fetch_snapshot_message(
        url: String,
        expected_epoch_min: Epoch,
    ) -> StdResult<Option<SnapshotMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<SnapshotMessage>().await {
                    Ok(snapshot) => match snapshot.beacon.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(snapshot)),
                        epoch => Err(anyhow!(
                            "Minimum expected snapshot epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(anyhow!(err).context("Invalid snapshot body")),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
        AttemptResult::Ok(snapshot) => {
            // todo: assert that the snapshot is really signed
            info!("Signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash);
            Ok(snapshot.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_node_producing_cardano_database_snapshot(
    aggregator_endpoint: &str,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-database");
    info!("Waiting for the aggregator to produce a Cardano database snapshot");

    async fn fetch_last_cardano_database_snapshot_merkle_root(
        url: String,
    ) -> StdResult<Option<String>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response
                    .json::<CardanoDatabaseSnapshotListMessage>()
                    .await
                    .as_deref()
                {
                    Ok([cardano_database_snapshot, ..]) => {
                        Ok(Some(cardano_database_snapshot.merkle_root.clone()))
                    }
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(anyhow!("Invalid Cardano database snapshot body : {err}",)),
                },
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    // todo: reduce the number of attempts if we can reduce the delay between two immutables
    match attempt!(45, Duration::from_millis(2000), {
        fetch_last_cardano_database_snapshot_merkle_root(url.clone()).await
    }) {
        AttemptResult::Ok(merkle_root) => {
            info!("Aggregator produced a Cardano database snapshot"; "merkle_root" => &merkle_root);
            Ok(merkle_root)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_cardano_database_snapshot(
    aggregator_endpoint: &str,
    merkle_root: &str,
    expected_epoch_min: Epoch,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-database/{merkle_root}");
    info!(
        "Asserting the aggregator is signing the Cardano database snapshot message `{}` with an expected min epoch of `{}`",
        merkle_root,
        expected_epoch_min
    );

    async fn fetch_cardano_database_snapshot_message(
        url: String,
        expected_epoch_min: Epoch,
    ) -> StdResult<Option<CardanoDatabaseSnapshotMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CardanoDatabaseSnapshotMessage>().await {
                    Ok(cardano_database_snapshot) => match cardano_database_snapshot.beacon.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(cardano_database_snapshot)),
                        epoch => Err(anyhow!(
                            "Minimum expected Cardano database snapshot epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(anyhow!(err).context("Invalid Cardano database snapshot body")),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_database_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
        AttemptResult::Ok(snapshot) => {
            info!("Signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash);
            Ok(snapshot.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{url}`"
        )),
    }
}

pub async fn assert_node_producing_cardano_transactions(
    aggregator_endpoint: &str,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-transactions");
    info!("Waiting for the aggregator to produce a Cardano transactions artifact");

    async fn fetch_last_cardano_transaction_snapshot_hash(
        url: String,
    ) -> StdResult<Option<String>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response
                    .json::<CardanoTransactionSnapshotListMessage>()
                    .await
                    .as_deref()
                {
                    Ok([artifact, ..]) => Ok(Some(artifact.hash.clone())),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(anyhow!(
                        "Invalid Cardano transactions artifact body : {err}",
                    )),
                },
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(45, Duration::from_millis(2000), {
        fetch_last_cardano_transaction_snapshot_hash(url.clone()).await
    }) {
        AttemptResult::Ok(hash) => {
            info!("Aggregator produced a Cardano transactions artifact"; "hash" => &hash);
            Ok(hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_transactions, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_cardano_transactions(
    aggregator_endpoint: &str,
    hash: &str,
    expected_epoch_min: Epoch,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-transaction/{hash}");
    info!(
        "Asserting the aggregator is signing the Cardano transactions artifact `{}` with an expected min epoch of `{}`",
        hash,
        expected_epoch_min
    );

    async fn fetch_cardano_transaction_snapshot_message(
        url: String,
        expected_epoch_min: Epoch,
    ) -> StdResult<Option<CardanoTransactionSnapshotMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CardanoTransactionSnapshotMessage>().await {
                    Ok(artifact) => match artifact.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(artifact)),
                        epoch => Err(anyhow!(
                            "Minimum expected artifact epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(anyhow!(err).context("Invalid Cardano transactions artifact body")),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_transaction_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
        AttemptResult::Ok(artifact) => {
            info!("Signer signed a Cardano transactions artifact"; "certificate_hash" => &artifact.certificate_hash);
            Ok(artifact.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_cardano_transactions, no response from `{url}`"
        )),
    }
}

pub async fn assert_node_producing_cardano_stake_distribution(
    aggregator_endpoint: &str,
) -> StdResult<(String, Epoch)> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-stake-distributions");
    info!("Waiting for the aggregator to produce a Cardano stake distribution");

    async fn fetch_last_cardano_stake_distribution_message(
        url: String,
    ) -> StdResult<Option<(String, Epoch)>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response
                    .json::<CardanoStakeDistributionListMessage>()
                    .await
                    .as_deref()
                {
                    Ok([stake_distribution, ..]) => Ok(Some((
                        stake_distribution.hash.clone(),
                        stake_distribution.epoch,
                    ))),
                    Ok(&[]) => Ok(None),
                    Err(err) => Err(anyhow!("Invalid Cardano stake distribution body : {err}",)),
                },
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(45, Duration::from_millis(2000), {
        fetch_last_cardano_stake_distribution_message(url.clone()).await
    }) {
        AttemptResult::Ok((hash, epoch)) => {
            info!("Aggregator produced a Cardano stake distribution"; "hash" => &hash, "epoch" => #?epoch);
            Ok((hash, epoch))
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_signer_is_signing_cardano_stake_distribution(
    aggregator_endpoint: &str,
    hash: &str,
    expected_epoch_min: Epoch,
) -> StdResult<String> {
    let url = format!("{aggregator_endpoint}/artifact/cardano-stake-distribution/{hash}");
    info!(
        "Asserting the aggregator is signing the Cardano stake distribution message `{}` with an expected min epoch of `{}`",
        hash,
        expected_epoch_min
    );

    async fn fetch_cardano_stake_distribution_message(
        url: String,
        expected_epoch_min: Epoch,
    ) -> StdResult<Option<CardanoStakeDistributionMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CardanoStakeDistributionMessage>().await {
                    Ok(stake_distribution) => match stake_distribution.epoch {
                        epoch if epoch >= expected_epoch_min => Ok(Some(stake_distribution)),
                        epoch => Err(anyhow!(
                            "Minimum expected Cardano stake distribution epoch not reached : {epoch} < {expected_epoch_min}"
                        )),
                    },
                    Err(err) => Err(anyhow!(err).context("Invalid Cardano stake distribution body",)),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_stake_distribution_message(url.clone(), expected_epoch_min).await
    }) {
        AttemptResult::Ok(cardano_stake_distribution) => {
            info!("Signer signed a Cardano stake distribution"; "certificate_hash" => &cardano_stake_distribution.certificate_hash);
            Ok(cardano_stake_distribution.certificate_hash)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_cardano_stake_distribution, no response from `{url}`"
        )),
    }
}

pub async fn assert_is_creating_certificate_with_enough_signers(
    aggregator_endpoint: &str,
    certificate_hash: &str,
    total_signers_expected: usize,
) -> StdResult<()> {
    let url = format!("{aggregator_endpoint}/certificate/{certificate_hash}");

    async fn fetch_certificate_message(url: String) -> StdResult<Option<CertificateMessage>> {
        match reqwest::get(url.clone()).await {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<CertificateMessage>().await {
                    Ok(certificate) => Ok(Some(certificate)),
                    Err(err) => Err(anyhow!(err).context("Invalid snapshot body")),
                },
                StatusCode::NOT_FOUND => Ok(None),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            },
            Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
        }
    }

    match attempt!(10, Duration::from_millis(1000), {
        fetch_certificate_message(url.clone()).await
    }) {
        AttemptResult::Ok(certificate) => {
            info!("Aggregator produced a certificate"; "certificate" => ?certificate);
            if certificate.metadata.signers.len() == total_signers_expected {
                info!(
                    "Certificate is signed by expected number of signers: {} >= {} ",
                    certificate.metadata.signers.len(),
                    total_signers_expected
                );
                Ok(())
            } else {
                Err(anyhow!(
                    "Certificate is not signed by expected number of signers: {} < {} ",
                    certificate.metadata.signers.len(),
                    total_signers_expected
                ))
            }
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_is_creating_certificate, no response from `{url}`"
        )),
    }
}

pub async fn assert_client_can_verify_snapshot(client: &mut Client, digest: &str) -> StdResult<()> {
    client
        .run(ClientCommand::CardanoDb(CardanoDbCommand::Download {
            digest: digest.to_string(),
        }))
        .await?;
    info!("Client downloaded & restored the snapshot"; "digest" => &digest);

    Ok(())
}

pub async fn assert_client_can_verify_mithril_stake_distribution(
    client: &mut Client,
    hash: &str,
) -> StdResult<()> {
    client
        .run(ClientCommand::MithrilStakeDistribution(
            MithrilStakeDistributionCommand::Download {
                hash: hash.to_owned(),
            },
        ))
        .await?;
    info!("Client downloaded the Mithril stake distribution"; "hash" => &hash);

    Ok(())
}

pub async fn assert_client_can_verify_transactions(
    client: &mut Client,
    tx_hashes: Vec<TransactionHash>,
) -> StdResult<()> {
    #[allow(dead_code)]
    #[derive(Debug, serde::Deserialize)]
    struct ClientCtxCertifyResult {
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
    let result: ClientCtxCertifyResult = serde_json::from_str(&file).with_context(|| {
        format!(
            "Failed to parse client output as json from file `{}`",
            result_file.display()
        )
    })?;

    info!("Asserting that all Cardano transactions where verified by the Client...");
    if tx_hashes
        .iter()
        .all(|tx| result.certified_transactions.contains(tx))
    {
        Ok(())
    } else {
        Err(anyhow!(
            "Not all transactions where certified:\n'{:#?}'",
            result,
        ))
    }
}

pub async fn assert_client_can_verify_cardano_stake_distribution(
    client: &mut Client,
    hash: &str,
    epoch: Epoch,
) -> StdResult<()> {
    client
        .run(ClientCommand::CardanoStakeDistribution(
            CardanoStakeDistributionCommand::Download {
                unique_identifier: epoch.to_string(),
            },
        ))
        .await?;
    info!("Client downloaded the Cardano stake distribution by epoch"; "epoch" => epoch.to_string());

    client
        .run(ClientCommand::CardanoStakeDistribution(
            CardanoStakeDistributionCommand::Download {
                unique_identifier: hash.to_string(),
            },
        ))
        .await?;
    info!("Client downloaded the Cardano stake distribution by hash"; "hash" => hash.to_string());

    Ok(())
}
