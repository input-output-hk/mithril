use std::error::Error;

use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::debug;
use cli_table::print_stdout;

use crate::{entities::SnapshotListItem, Runtime, Config};

/// List snapshots command
#[derive(Parser, Debug, Clone)]
pub struct ListCommand {
    /// Does the output need to be in JSON format?
    #[clap(long)]
    json: bool,
}

impl ListCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("List snapshots");
                    let config: Config = config_builder.build()?;
                    let runtime = Runtime::new(config.network);
                    let snapshot_list_items = runtime.list_snapshots().await?;

                    if self.json {
                        println!("{}", serde_json::to_string(&snapshot_list_items).unwrap());
                    } else {
                        print_stdout(snapshot_list_items.with_title()).unwrap();
                    }

                    Ok(())
        let runtime = Runtime::new(config.network);
        todo!()
    }
}
/*

#[cfg(test)]
mod tests {
    fn test_list_snapshots_ok() {
        let mock_aggregator_handler = MockAggregatorHandlerImpl::new();
    }
}
 */
