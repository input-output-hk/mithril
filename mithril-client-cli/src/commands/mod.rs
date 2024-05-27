//! Command module
//! This module holds the subcommands that can be used from the CLI.
//!
//!

pub mod cardano_db;
pub mod cardano_transaction;
mod deprecation;
pub mod mithril_stake_distribution;

pub use deprecation::{DeprecatedCommand, Deprecation};

use mithril_client::{ClientBuilder, MithrilResult};
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
    let fallback_genesis_verification_key = "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138\
        382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c323\
        5322c3138302c37322c3133342c3133372c3234372c3136312c36385d";

    let builder = ClientBuilder::aggregator(
        &params.require("aggregator_endpoint")?,
        &params.get_or(
            "genesis_verification_key",
            fallback_genesis_verification_key,
        ),
    )
    .with_logger(logger());

    Ok(builder)
}
