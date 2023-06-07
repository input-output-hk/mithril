use std::sync::Arc;

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, Config, ConfigBuilder};
use mithril_common::StdResult;

use crate::{dependencies::DependenciesBuilder, MithrilStakeDistributionListItem};

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
        let config: Config = config_builder.build()?;
        let mut dependencies_builder = DependenciesBuilder::new(Arc::new(config));
        let service = dependencies_builder
            .get_mithril_stake_distribution_service()
            .await?;
        let lines = service.list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&lines)?);
        } else {
            let lines = lines
                .into_iter()
                .map(MithrilStakeDistributionListItem::from)
                .collect::<Vec<_>>();
            print_stdout(lines.with_title())?;
        }

        Ok(())
    }
}
