use clap::Parser;
use cli_table::{format::Justify, print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use std::collections::HashMap;

use crate::{commands::client_builder_with_fallback_genesis_key, configuration::ConfigParameters};
use mithril_client::MithrilResult;

/// Clap command to list existing cardano dbs
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl CardanoDbListCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        let config = config_builder.build()?;
        let params = ConfigParameters::new(config.try_deserialize::<HashMap<String, String>>()?);
        let client = client_builder_with_fallback_genesis_key(&params)?.build()?;
        let items = client.snapshot().list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&items)?);
        } else {
            let items = items
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.beacon.epoch).cell(),
                        format!("{}", item.beacon.immutable_file_number).cell(),
                        item.beacon.network.cell(),
                        item.digest.cell(),
                        item.size.cell(),
                        format!("{}", item.locations.len()).cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Immutable".cell(),
                    "Network".cell(),
                    "Digest".cell(),
                    "Size".cell().justify(Justify::Right),
                    "Locations".cell().justify(Justify::Right),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(items)?;
        }

        Ok(())
    }
}
