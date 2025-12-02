use clap::Parser;
use cli_table::{Cell, Table, format::Justify, print_stdout};

use crate::{CommandContext, commands::client_builder_with_fallback_genesis_key};
use mithril_client::{
    MithrilResult, RequiredAggregatorCapabilities, common::SignedEntityTypeDiscriminants,
};

/// Cardano stake distribution LIST command
#[derive(Parser, Debug, Clone)]
pub struct CardanoStakeDistributionListCommand {}

impl CardanoStakeDistributionListCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let client = client_builder_with_fallback_genesis_key(context.config_parameters())?
            .with_capabilities(RequiredAggregatorCapabilities::SignedEntityType(
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ))
            .with_logger(context.logger().clone())
            .build()?;
        let lines = client.cardano_stake_distribution().list().await?;

        if context.is_json_output_enabled() {
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
