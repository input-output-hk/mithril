//! Commands for the Cardano db artifact
mod download;
mod list;
mod shared_steps;
mod show;
mod verify;

pub use download::*;
pub use list::*;
pub use show::*;
pub use verify::*;

use crate::CommandContext;
use clap::{Subcommand, ValueEnum};
use mithril_client::MithrilResult;

/// Backend to use for Cardano Database commands
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, ValueEnum)]
pub enum CardanoDbCommandsBackend {
    /// Legacy backend
    #[clap(help = "(deprecated) Legacy backend, full database restoration only")]
    V1,
    /// V2 backend
    #[default]
    #[clap(help = "[default] V2 backend, full or partial database restoration")]
    V2,
}

/// Cardano db management (alias: cdb)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbCommands {
    /// Cardano db snapshot commands
    #[clap(subcommand)]
    Snapshot(CardanoDbSnapshotCommands),

    /// Download a Cardano db snapshot and verify its associated certificate
    #[clap(arg_required_else_help = true)]
    Download(CardanoDbDownloadCommand),

    /// Verify a Cardano database content
    #[clap(arg_required_else_help = true)]
    Verify(CardanoDbVerifyCommand),
}

/// Cardano db snapshots
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbSnapshotCommands {
    /// List available Cardano db snapshots
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed information about a Cardano db snapshot
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),
}

impl CardanoDbCommands {
    /// Execute Cardano db command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Download(cmd) => cmd.execute(context).await,
            Self::Snapshot(cmd) => cmd.execute(context).await,
            Self::Verify(cmd) => cmd.execute(context).await,
        }
    }
}

impl CardanoDbSnapshotCommands {
    /// Execute Cardano db snapshot command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Print in stderr a warning about the deprecation of the v1 backend and its scheduled removal in 2026
pub fn warn_deprecated_v1_backend(context: &CommandContext) {
    use crate::utils::JSON_CAUTION_KEY;

    let message = "The `v1` backend is deprecated and is scheduled to be removed early 2026. \
    Please use the `v2` backend instead. \
    No other change is required in your command line.";

    if context.is_json_output_enabled() {
        eprintln!(r#"{{"{JSON_CAUTION_KEY}":"{message}"}}"#);
    } else {
        eprintln!("Warning: {message}");
    }
}

/// Print in stderr that the given parameters are not available with the v1 backend and will be ignored
pub fn warn_unused_parameter_with_v1_backend<const N: usize>(
    context: &CommandContext,
    v2_only_parameters: [&str; N],
) {
    use crate::utils::JSON_CAUTION_KEY;

    let message = format_unused_parameter_with_v1_backend(v2_only_parameters);
    if context.is_json_output_enabled() {
        eprintln!(r#"{{"{JSON_CAUTION_KEY}":"{message}"}}"#);
    } else {
        eprintln!("{message}");
        // Add a blank line to separate this message from the one related to the fast bootstrap that comes next.
        eprintln!();
    }
}

fn format_unused_parameter_with_v1_backend<const N: usize>(
    v2_only_parameters: [&str; N],
) -> String {
    match v2_only_parameters.len() {
        0 => String::new(),
        1 => format!(
            "`{}` is only available with the `v2` backend. It will be ignored.",
            v2_only_parameters[0]
        ),
        n => {
            format!(
                "`{}`, and `{}` are only available with the `v2` backend. They will be ignored.",
                v2_only_parameters[..n - 1].join("`, `"),
                v2_only_parameters[n - 1]
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_unused_parameter_with_v1_backend() {
        assert_eq!(format_unused_parameter_with_v1_backend([]), String::new());

        assert_eq!(
            format_unused_parameter_with_v1_backend(["--test"]),
            "`--test` is only available with the `v2` backend. It will be ignored.".to_string()
        );

        assert_eq!(
            format_unused_parameter_with_v1_backend(["--foo", "--bar"]),
            "`--foo`, and `--bar` are only available with the `v2` backend. They will be ignored."
                .to_string()
        );

        assert_eq!(
            format_unused_parameter_with_v1_backend(["--test", "--foo", "--bar"]),
            "`--test`, `--foo`, and `--bar` are only available with the `v2` backend. They will be ignored.".to_string()
        );
    }
}
