use config::builder::DefaultState;
use config::ConfigBuilder;
use slog::Logger;
use std::collections::HashMap;

use mithril_client::MithrilResult;

use crate::configuration::ConfigParameters;

/// Context for the command execution
pub struct CommandContext {
    config_builder: ConfigBuilder<DefaultState>,
    logger: Logger,
}

impl CommandContext {
    /// Create a new command context
    pub fn new(config_builder: ConfigBuilder<DefaultState>, logger: Logger) -> Self {
        Self {
            config_builder,
            logger,
        }
    }

    /// Get the configured parameters
    pub fn config_parameters(&self) -> MithrilResult<ConfigParameters> {
        let config = self.config_builder.clone().build()?;
        let config_hash_map = config.try_deserialize::<HashMap<String, String>>()?;
        Ok(ConfigParameters::new(config_hash_map))
    }

    /// Get the shared logger
    pub fn logger(&self) -> &Logger {
        &self.logger
    }
}
