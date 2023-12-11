use clap::Parser;
use cli_table::{format::Justify, print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::logger;
use std::{collections::HashMap, sync::Arc};

use mithril_client::ClientBuilder;
use mithril_client_cli::configuration::ConfigParameters;
use mithril_common::StdResult;

/// Mithril stake distribution LIST command
#[derive(Parser, Debug, Clone)]
pub struct MithrilStakeDistributionListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl MithrilStakeDistributionListCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config = config_builder.build()?;
        let params = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        let aggregator_endpoint = &params.require("aggregator_endpoint")?;
        let genesis_verification_key = &params.require("genesis_verification_key")?;
        let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
            .with_logger(logger())
            .build()?;
        let lines = client.mithril_stake_distribution().list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&lines)?);
        } else {
            let lines = lines
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.epoch).cell(),
                        item.hash.cell(),
                        item.certificate_hash.cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Hash".cell(),
                    "Certificate Hash".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(lines)?;
        }

        Ok(())
    }
}
