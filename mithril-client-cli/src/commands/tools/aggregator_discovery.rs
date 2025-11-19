use clap::Parser;

use mithril_client::{AggregatorDiscoveryType, ClientBuilder, MithrilNetwork, MithrilResult};

/// Clap command to select an aggregator from the available ones with automatic discovery.
#[derive(Parser, Debug, Clone)]
pub struct AggregatorSelectCommand {
    /// Path to the Cardano node database directory.
    #[clap(long)]
    network: MithrilNetwork,
}

impl AggregatorSelectCommand {
    /// Main command execution
    pub async fn execute(&self) -> MithrilResult<()> {
        let aggregator_endpoints =
            ClientBuilder::new(AggregatorDiscoveryType::Automatic(self.network.to_owned()))
                .with_default_aggregator_discoverer()
                .discover_aggregator(&self.network)?;

        for endpoint in aggregator_endpoints.iter() {
            println!("Discovered aggregator endpoint: {:?}", endpoint);
        }

        Ok(())
    }
}
