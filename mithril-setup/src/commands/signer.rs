//! Command to setup a Mithril signer

use clap::Parser;
use slog::info;

use crate::CommandContext;

/// Set up a Mithril signer
#[derive(Parser, Debug, Clone)]
pub struct SignerCommand {}

impl SignerCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> anyhow::Result<()> {
        context.require_unstable("signer", None)?;
        info!(context.logger(), "signer command");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_logger() -> slog::Logger {
        slog::Logger::root(slog::Discard, slog::o!())
    }

    #[tokio::test]
    async fn signer_command_returns_error_when_unstable_is_not_enabled() {
        let context = CommandContext::new(false, false, test_logger());
        let cmd = SignerCommand {};
        assert!(cmd.execute(context).await.is_err());
    }
}
