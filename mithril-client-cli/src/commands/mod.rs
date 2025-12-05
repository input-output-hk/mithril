//! Command module
//! This module holds the subcommands that can be used from the CLI.
//!
//!

pub mod cardano_db;
pub mod cardano_stake_distribution;
pub mod cardano_transaction;
mod deprecation;
pub mod mithril_stake_distribution;
pub mod tools;

pub use deprecation::{DeprecatedCommand, Deprecation};

use std::{str::FromStr, sync::Arc};

use mithril_client::{
    AggregatorDiscoveryType, ClientBuilder, GenesisVerificationKey, MithrilResult,
};

use crate::{configuration::ConfigParameters, utils::ForcedEraFetcher};

const CLIENT_TYPE_CLI: &str = "CLI";

pub(crate) fn client_builder(params: &ConfigParameters) -> MithrilResult<ClientBuilder> {
    let builder = ClientBuilder::new(AggregatorDiscoveryType::from_str(
        &params.require("aggregator_endpoint")?,
    )?)
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(
        params.require("genesis_verification_key")?,
    ));

    Ok(finalize_builder_config(builder, params))
}

pub(crate) fn client_builder_with_fallback_genesis_key(
    params: &ConfigParameters,
) -> MithrilResult<ClientBuilder> {
    // TODO: This should not be done this way.
    // Now that mithril-client-cli uses the mithril-client library, the genesis verification key is required for all commands
    let fallback_genesis_verification_key = "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138\
        382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c323\
        5322c3138302c37322c3133342c3133372c3234372c3136312c36385d";

    let builder = ClientBuilder::new(AggregatorDiscoveryType::from_str(
        &params.require("aggregator_endpoint")?,
    )?)
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(params.get_or(
        "genesis_verification_key",
        fallback_genesis_verification_key,
    )));

    Ok(finalize_builder_config(builder, params))
}

fn finalize_builder_config(mut builder: ClientBuilder, params: &ConfigParameters) -> ClientBuilder {
    builder = builder
        .with_origin_tag(params.get("origin_tag"))
        .with_client_type(Some(CLIENT_TYPE_CLI.to_string()));

    if let Some(era) = params.get("era") {
        builder = builder.with_era_fetcher(Arc::new(ForcedEraFetcher::new(era.to_string())));
    }

    builder
}
