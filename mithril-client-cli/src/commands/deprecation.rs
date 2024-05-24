use clap::{
    builder::{StyledStr, Styles},
    error::{ContextKind, ContextValue},
};

use crate::ClapError;

/// Stores the deprecated command name and the new command name to use.
#[derive(Clone)]
pub struct DeprecatedCommand {
    command: String,
    new_command: String,
}

impl DeprecatedCommand {
    /// Create information about a deprecated command
    pub fn new<S: ToString>(command: S, new_command: S) -> Self {
        Self {
            command: command.to_string(),
            new_command: new_command.to_string(),
        }
    }
}

/// Tool to handle deprecated Clap commands.
pub struct Deprecation;

impl Deprecation {
    fn find_deprecated_command(
        error: &ClapError,
        deprecated_commands: Vec<DeprecatedCommand>,
    ) -> Option<DeprecatedCommand> {
        if let Some(context_value) = error.get(ContextKind::InvalidSubcommand) {
            let command_name = context_value.to_string();
            deprecated_commands
                .into_iter()
                .find(|dc| command_name == dc.command)
        } else {
            None
        }
    }

    /// Modify result to add information on deprecated commands.
    pub fn handle_deprecated_commands<A>(
        matches_result: Result<A, ClapError>,
        styles: Styles,
        deprecated_commands: Vec<DeprecatedCommand>,
    ) -> Result<A, ClapError> {
        matches_result.map_err(|mut e: ClapError| {
            if let Some(deprecated_command) = Self::find_deprecated_command(&e, deprecated_commands)
            {
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
    use clap::error::{ContextKind, ContextValue, ErrorKind};
    use clap::{CommandFactory, Parser, Subcommand};

    #[derive(Parser, Debug, Clone)]
    pub struct MyCommand {
        #[clap(subcommand)]
        command: MySubCommands,
    }

    #[derive(Subcommand, Debug, Clone)]
    enum MySubCommands {}

    #[test]
    fn invalid_sub_command_message_for_a_non_deprecated_command_is_not_modified() {
        fn build_error() -> Result<MyCommand, ClapError> {
            let mut e =
                ClapError::new(ErrorKind::InvalidSubcommand).with_cmd(&MyCommand::command());

            e.insert(
                ContextKind::InvalidSubcommand,
                ContextValue::String("invalid_command".to_string()),
            );

            Err(e)
        }

        let default_error_message = build_error().err().unwrap().to_string();

        let result = Deprecation::handle_deprecated_commands(
            build_error(),
            Styles::plain(),
            vec![DeprecatedCommand::new("old_command", "new_command")],
        );
        assert!(result.is_err());
        let message = result.err().unwrap().to_string();
        assert_eq!(default_error_message, message);
    }

    #[test]
    fn replace_error_message_on_deprecated_commands_and_show_the_new_command() {
        let mut e = ClapError::new(ErrorKind::InvalidSubcommand).with_cmd(&MyCommand::command());
        e.insert(
            ContextKind::InvalidSubcommand,
            ContextValue::String("old_command".to_string()),
        );

        let result = Deprecation::handle_deprecated_commands(
            Err(e) as Result<MyCommand, ClapError>,
            Styles::plain(),
            vec![DeprecatedCommand::new("old_command", "new_command")],
        );
        assert!(result.is_err());
        let message = result.err().unwrap().to_string();
        assert!(message.contains("'old_command'"));
        assert!(message.contains("deprecated"));
        assert!(message.contains("'new_command'"));
    }
}
