use anyhow::anyhow;
use config::ConfigBuilder;
use config::builder::DefaultState;
use slog::Logger;
use std::collections::HashMap;

use mithril_client::MithrilResult;

use crate::configuration::ConfigParameters;

/// Context for the command execution
pub struct CommandContext {
    config_builder: ConfigBuilder<DefaultState>,
    unstable_enabled: bool,
    json: bool,
    logger: Logger,
}

impl CommandContext {
    /// Create a new command context
    pub fn new(
        config_builder: ConfigBuilder<DefaultState>,
        unstable_enabled: bool,
        json: bool,
        logger: Logger,
    ) -> Self {
        Self {
            config_builder,
            unstable_enabled,
            json,
            logger,
        }
    }

    /// Check if unstable commands are enabled
    pub fn is_unstable_enabled(&self) -> bool {
        self.unstable_enabled
    }

    /// Check if JSON output is enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.json
    }

    /// Ensure that unstable commands are enabled
    pub fn require_unstable(
        &self,
        sub_command: &str,
        command_example: Option<&str>,
    ) -> MithrilResult<()> {
        if self.is_unstable_enabled() {
            Ok(())
        } else {
            let example = command_example.map(|e| format!(" {e}")).unwrap_or_default();
            Err(anyhow!(
                "The \"{sub_command}\" subcommand is only accepted using the --unstable flag.\n\n\
                ie: \"mithril-client --unstable {sub_command}{example}\""
            ))
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

#[cfg(test)]
mod tests {
    use slog::o;

    use super::*;

    #[test]
    fn require_unstable_return_ok_if_unstable_enabled() {
        let unstable_enabled = true;
        let context = CommandContext::new(
            ConfigBuilder::default(),
            unstable_enabled,
            true,
            Logger::root(slog::Discard, o!()),
        );

        let result = context.require_unstable("test", None);
        assert!(result.is_ok(), "Expected Ok, got {result:?}");
    }

    #[test]
    fn require_unstable_return_err_if_unstable_disabled() {
        let unstable_enabled = false;
        let context = CommandContext::new(
            ConfigBuilder::default(),
            unstable_enabled,
            true,
            Logger::root(slog::Discard, o!()),
        );

        let result = context.require_unstable("test", None);
        assert!(result.is_err(), "Expected Err, got {result:?}");
    }
}
