use clap::Subcommand;
use clap::{
    builder::{StyledStr, Styles},
    error::{ContextKind, ContextValue},
};

pub struct DeprecatedCommand {
    pub command: String,
    pub new_command: String,
}

pub struct Deprecation;

impl Deprecation {
    pub fn handle_deprecated_commands<A>(
        matches_result: Result<A, clap::error::Error>,
        styles: Styles,
        deprecated_commands: Vec<DeprecatedCommand>,
    ) -> Result<A, clap::error::Error> {
        matches_result.map_err(|mut e: clap::error::Error| {
            fn get_deprecated_command(
                error: &clap::error::Error,
                deprecated_commands: Vec<DeprecatedCommand>,
            ) -> Option<DeprecatedCommand> {
                if let Some(context_value) = error.get(ContextKind::InvalidSubcommand) {
                    let command = context_value.to_string();
                    for deprecated_command in deprecated_commands {
                        if command == deprecated_command.command {
                            return Some(deprecated_command);
                        }
                    }
                }
                None
            }
            if let Some(deprecated_command) = get_deprecated_command(&e, deprecated_commands) {
                let message = format!(
                    "'{}{}{}' command is deprecated, use '{}{}{}' command instead",
                    styles.get_error().render(),
                    deprecated_command.command,
                    styles.get_error().render_reset(),
                    styles.get_valid().render(),
                    deprecated_command.new_command,
                    styles.get_valid().render_reset(),
                );
                e.insert(
                    ContextKind::Suggested,
                    ContextValue::StyledStrs(vec![StyledStr::from(&message)]),
                );
            }
            e
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{
        builder::StyledStr,
        error::{ContextKind, ContextValue, ErrorKind},
        FromArgMatches,
    };
    use clap::{ArgMatches, CommandFactory, Parser, Subcommand};

    #[derive(Parser, Debug, Clone)]
    // #[command(version)]
    pub struct MyCmd {
        /// Available commands
        #[clap(subcommand)]
        command: MySubCommands,
    }

    #[derive(Subcommand, Debug, Clone)]
    enum MySubCommands {
        #[clap(subcommand)]
        CardanoDb,

        #[clap(subcommand)]
        MithrilStakeDistribution,
    }

    impl MyCmd {
        pub async fn execute(&self) {}
    }

    #[test]
    fn XXXX_replace_error_message_on_deprecated_commands() {
        {
            let mut e = clap::error::Error::new(clap::error::ErrorKind::InvalidSubcommand)
                .with_cmd(&MyCmd::command());
            e.insert(
                ContextKind::InvalidSubcommand,
                ContextValue::String("deprecated_command".to_string()),
            );
            let result = Deprecation::handle_deprecated_commands(
                Err(e) as Result<MyCmd, clap::error::Error>,
                Styles::plain(),
                vec![DeprecatedCommand {
                    command: "deprecated_other_command".to_string(),
                    new_command: "new_command".to_string(),
                }],
            );
            assert!(result.is_err());
            let message = result.err().unwrap().to_string();
            assert!(message.contains("'deprecated_command'"));
            assert!(!message.contains("'new_command'"));
        }
        {
            let mut e = clap::error::Error::new(clap::error::ErrorKind::InvalidSubcommand)
                .with_cmd(&MyCmd::command());
            e.insert(
                ContextKind::InvalidSubcommand,
                ContextValue::String("deprecated_command".to_string()),
            );

            let result = Deprecation::handle_deprecated_commands(
                Err(e) as Result<MyCmd, clap::error::Error>,
                Styles::plain(),
                vec![DeprecatedCommand {
                    command: "deprecated_command".to_string(),
                    new_command: "new_command".to_string(),
                }],
            );
            assert!(result.is_err());
            let message = result.err().unwrap().to_string();
            assert!(message.contains("'deprecated_command'"));
            assert!(message.contains("'new_command'"));
        }
    }
}
