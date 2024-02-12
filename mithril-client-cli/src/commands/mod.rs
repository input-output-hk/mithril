//! Command module
//! This module holds the subcommands that can be used from the CLI.
//!
//!

pub mod cardano_transaction;
pub mod mithril_stake_distribution;
pub mod snapshot;

use mithril_client::{ClientBuilder, MithrilResult};
use mithril_common::test_utils::fake_keys;
use slog_scope::logger;

use crate::configuration::ConfigParameters;

pub(crate) fn client_builder(params: &ConfigParameters) -> MithrilResult<ClientBuilder> {
    let builder = ClientBuilder::aggregator(
        &params.require("aggregator_endpoint")?,
        &params.require("genesis_verification_key")?,
    )
    .with_logger(logger());

    Ok(builder)
}

pub(crate) fn client_builder_with_fallback_genesis_key(
    params: &ConfigParameters,
) -> MithrilResult<ClientBuilder> {
    // TODO: This should not be done this way.
    // Now that mithril-client-cli uses the mithril-client library, the genesis verification key is required for all commands
    let fallback_genesis_verification_key = fake_keys::genesis_verification_key()[0].to_string();

    let builder = ClientBuilder::aggregator(
        &params.require("aggregator_endpoint")?,
        &params.get_or(
            "genesis_verification_key",
            &fallback_genesis_verification_key,
        ),
    )
    .with_logger(logger());

    Ok(builder)
}
