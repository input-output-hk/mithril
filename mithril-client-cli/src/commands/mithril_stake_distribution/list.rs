use clap::Parser;
use cli_table::{Cell, Table, format::Justify, print_stdout};

use crate::{
    CommandContext,
    commands::{SharedArgs, client_builder_with_fallback_genesis_key},
};
use mithril_client::MithrilResult;

/// Mithril stake distribution LIST command
#[derive(Parser, Debug, Clone)]
pub struct MithrilStakeDistributionListCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,
}

impl MithrilStakeDistributionListCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?;
        let client = client_builder_with_fallback_genesis_key(&params)?
            .with_logger(context.logger().clone())
            .build()?;
        let lines = client.mithril_stake_distribution().list().await?;

        if self.is_json_output_enabled() {
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
