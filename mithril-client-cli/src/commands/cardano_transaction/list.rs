use clap::Parser;
use cli_table::format::Justify;
use cli_table::{print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_client::ClientBuilder;
use slog_scope::logger;
use std::collections::HashMap;
use std::sync::Arc;

use crate::configuration::ConfigParameters;
use mithril_common::test_utils::fake_keys;
use mithril_common::StdResult;

/// Cardano transaction commitment LIST command
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionCommitmentListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl CardanoTransactionCommitmentListCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config = config_builder.build()?;
        let params = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        // TODO: This should not be done this way.
        // Now that mithril-client-cli uses the mithril-client library, the genesis verification key is required for all commands
        let fallback_genesis_verification_key =
            fake_keys::genesis_verification_key()[0].to_string();
        let client = ClientBuilder::aggregator(
            &params.require("aggregator_endpoint")?,
            &params.get_or(
                "genesis_verification_key",
                &fallback_genesis_verification_key,
            ),
        )
        .with_logger(logger())
        .build()?;
        let lines = client.cardano_transaction_proof().list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&lines)?);
        } else {
            let lines = lines
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.beacon.epoch).cell(),
                        format!("{}", item.beacon.immutable_file_number).cell(),
                        item.beacon.network.cell(),
                        item.hash.cell(),
                        item.certificate_hash.cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Immutable".cell(),
                    "Network".cell(),
                    "Hash".cell(),
                    "Certificate Hash".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(lines)?;
        }

        Ok(())
    }
}
