use config::builder::DefaultState;
use config::ConfigBuilder;
use slog::Logger;
use std::collections::HashMap;

use mithril_client::MithrilResult;

use crate::configuration::ConfigParameters;

/// Context for the command execution
pub struct CommandContext {
    config_builder: ConfigBuilder<DefaultState>,
    unstable_enabled: bool,
    logger: Logger,
}

impl CommandContext {
    /// Create a new command context
    pub fn new(
        config_builder: ConfigBuilder<DefaultState>,
        unstable_enabled: bool,
        logger: Logger,
    ) -> Self {
        Self {
            config_builder,
            unstable_enabled,
            logger,
        }
    }

    /// Check if unstable commands are enabled
    pub fn is_unstable_enabled(&self) -> bool {
        self.unstable_enabled
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
