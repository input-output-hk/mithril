//! This example shows how to implement a Mithril client and use its features.
//!
//! In this example, the client interacts by default with a real aggregator (`release-preprod`) to get the data.

use anyhow::anyhow;
use clap::Parser;
use slog::info;
use std::sync::Arc;

use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};

#[derive(Parser, Debug)]
#[command(version)]
pub struct Args {
    /// Genesis verification key.
    #[clap(
        long,
        env = "GENESIS_VERIFICATION_KEY",
        default_value = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
    )]
    genesis_verification_key: String,

    /// Aggregator endpoint URL.
    #[clap(
        long,
        env = "AGGREGATOR_ENDPOINT",
        default_value = "https://aggregator.release-preprod.api.mithril.network/aggregator"
    )]
    aggregator_endpoint: String,
}

#[tokio::main]
async fn main() -> MithrilResult<()> {
    let args = Args::parse();
    let logger = build_logger();
    let client =
        ClientBuilder::aggregator(&args.aggregator_endpoint, &args.genesis_verification_key)
            .with_logger(logger.clone())
            .build()?;

    let cardano_stake_distributions = client.cardano_stake_distribution().list().await?;
    info!(
        logger,
        "Fetched Cardano stake distributions:\n{cardano_stake_distributions:#?}",
    );

    let last_epoch = cardano_stake_distributions
        .first()
        .ok_or(anyhow!(
            "No Cardano stake distributions could be listed from aggregator: '{}'",
            args.aggregator_endpoint
        ))?
        .epoch;

    let cardano_stake_distribution = client
        .cardano_stake_distribution()
        .get_by_epoch(last_epoch)
        .await?
        .ok_or(anyhow!(
            "A Cardano stake distribution should exist for hash '{last_epoch}'"
        ))?;
    info!(
        logger,
        "Fetched details of last Cardano stake distribution:\n{cardano_stake_distribution:#?}",
    );

    info!(
        logger,
        "Checking certificate chain of the last Cardano stake distribution ...",
    );
    let certificate = client
        .certificate()
        .verify_chain(&cardano_stake_distribution.certificate_hash)
        .await?;
    info!(
        logger,
        "Certificate chain is valid, checking that the last certificate, hash '{}', \
        effectively matches Cardano stake distribution '{}'",
        certificate.hash,
        cardano_stake_distribution.hash
    );

    let message = MessageBuilder::new()
        .compute_cardano_stake_distribution_message(&certificate, &cardano_stake_distribution)?;

    if certificate.match_message(&message) {
        info!(
            logger,
            "Certificate '{}' matches Cardano stake distribution '{}'",
            certificate.hash,
            cardano_stake_distribution.hash
        );
        Ok(())
    } else {
        Err(anyhow::anyhow!(
            "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
            certificate.signed_message,
            message.compute_hash()
        ))
    }
}

fn build_logger() -> slog::Logger {
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    slog::Logger::root(Arc::new(drain), slog::o!())
}
