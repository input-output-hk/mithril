use config::builder::DefaultState;
use config::ConfigBuilder;
use slog::Logger;

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
    pub fn config_builder(&self) -> ConfigBuilder<DefaultState> {
        self.config_builder.clone()
    }

    /// Get the shared logger
    pub fn logger(&self) -> &Logger {
        &self.logger
    }
}
