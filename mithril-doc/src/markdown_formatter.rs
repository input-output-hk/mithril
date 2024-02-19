use clap::{builder::StyledStr, Arg, Command};

use crate::extract_clap_info;

use super::StructDoc;

mod markdown {
    /// Format a list of label and a list of text list into a markdown table.
    pub fn format_table(header: &[&str], lines: &[Vec<String>]) -> String {
        format!(
            "{}\n{}",
            format_table_header(header),
            lines
                .iter()
                .map(|line| format_table_line(line))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }

    /// Format a list of text as a markdown table line.
    pub fn format_table_line(data: &[String]) -> String {
        format!("| {} |", data.join(" | "))
    }

    /// Format a list of label to a markdown header.
    /// To align the text to left, right or to center it, you need to add `:` at left, right or both.
    /// Example:  :Description:
    pub fn format_table_header(data: &[&str]) -> String {
        let headers = data
            .iter()
            .map(|header| {
                let align_left = header.chars().next().map(|c| c == ':').unwrap_or(false);
                let align_right = header.chars().last().map(|c| c == ':').unwrap_or(false);
                let label = &header[(if align_left { 1 } else { 0 })
                    ..(header.len() - (if align_right { 1 } else { 0 }))];
                (label, align_left, align_right)
            })
            .collect::<Vec<(&str, bool, bool)>>();

        let sublines = headers
            .iter()
            .map(|(label, left, right)| {
                format!(
                    "{}{}{}",
                    if *left { ":" } else { "-" },
                    "-".repeat(label.len()),
                    if *right { ":" } else { "-" }
                )
            })
            .collect::<Vec<String>>();

        let labels = headers
            .iter()
            .map(|(label, _, _)| label.to_string())
            .collect::<Vec<String>>();

        format!("| {} |\n|{}|", labels.join(" | "), sublines.join("|"))
    }
}

pub fn doc_markdown_with_config(cmd: &mut Command, struct_doc: Option<&StructDoc>) -> String {
    // See: https://github1s.com/clap-rs/clap/blob/HEAD/clap_builder/src/builder/command.rs#L1989

    fn format_parameters(
        cmd: &Command,
        struct_doc: Option<&StructDoc>,
        parameters_explanation: &str,
    ) -> String {
        if cmd
            .get_arguments()
            .filter(|arg| argument_to_document(arg))
            .peekable()
            .peek()
            .is_some()
        {
            let mut command_parameters = extract_clap_info::extract_parameters(cmd);
            if let Some(config_doc) = struct_doc {
                if !config_doc.data.is_empty() {
                    command_parameters = command_parameters.merge_struct_doc(config_doc);
                }
            }

            let parameters_table = doc_config_to_markdown(&command_parameters);

            format!("{}\n{}", parameters_explanation, parameters_table)
        } else {
            "".to_string()
        }
    }

    fn format_subcommand(cmd: &Command) -> String {
        let sub_commands = &mut cmd.get_subcommands().peekable();
        if sub_commands.peek().is_some() {
            let subcommands_lines = sub_commands
                .map(|command| {
                    vec![
                        format!("**{}**", command.get_name()),
                        command.get_about().map_or("".into(), StyledStr::to_string),
                    ]
                })
                .collect::<Vec<Vec<String>>>();

            markdown::format_table(&["Subcommand", "Performed action"], &subcommands_lines)
        } else {
            String::from("")
        }
    }

    fn format_command(
        cmd: &mut Command,
        parent: Option<String>,
        struct_doc: Option<&StructDoc>,
        level: usize,
        parameters_explanation: &str,
    ) -> String {
        // The Command object need to be completed calling `render_help` function.
        // Otherwise there is no parameter default values and the `help` command is absent.

        let help = format!("```bash\n{}\n```", cmd.render_long_help()); // More readable than help
        format_command_internal(cmd, parent, help, struct_doc, level, parameters_explanation)
    }

    fn name_to_document(name: &str) -> bool {
        name != "help"
    }

    fn argument_to_document(arg: &Arg) -> bool {
        name_to_document(arg.get_id().as_str())
    }

    fn command_to_document(cmd: &Command) -> bool {
        name_to_document(cmd.get_name())
    }

    fn format_command_internal(
        cmd: &Command,
        parent: Option<String>,
        help: String,
        struct_doc: Option<&StructDoc>,
        level: usize,
        parameters_explanation: &str,
    ) -> String {
        let parent_ancestors = parent.clone().map_or("".into(), |s| format!("{} ", s));
        let title = format!(
            "{} {}{}\n",
            "#".repeat(level),
            parent_ancestors,
            cmd.get_name()
        );
        let description = cmd.get_about().map_or("".into(), StyledStr::to_string);

        let subcommands_table = format_subcommand(cmd);

        let parameters = format_parameters(cmd, struct_doc, parameters_explanation);

        let subcommands = cmd
            .get_subcommands()
            .filter(|cmd| command_to_document(cmd))
            .map(|sub_command: &Command| {
                format_command(
                    &mut sub_command.clone(),
                    Some(format!("{} {}", parent_ancestors, cmd.get_name())),
                    None,
                    level + 1,
                    "",
                )
            })
            .collect::<Vec<String>>()
            .join("\n");

        format!("{title}\n{description}\n{help}\n{subcommands_table}\n{parameters}\n{subcommands}")
    }

    let parameters_explanation = "\n\
                The configuration parameters can be set in either of the following ways:\n\
                \n\
                1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.\n\
                \n\
                2. The value can be overridden by an environment variable with the parameter name in uppercase.\n\
                ";
    format_command(cmd, None, struct_doc, 3, parameters_explanation)
}

pub fn doc_config_to_markdown(struct_doc: &StructDoc) -> String {
    let subcommands_lines = struct_doc
        .data
        .iter()
        .map(|config| {
            let config = config.clone();
            vec![
                format!("`{}`", config.parameter),
                if config.command_line_long.is_empty() {
                    "-".to_string()
                } else {
                    format!("`{}`", config.command_line_long)
                },
                if config.command_line_short.is_empty() {
                    "-".to_string()
                } else {
                    format!("`{}`", config.command_line_short)
                },
                config
                    .environment_variable
                    .map_or_else(|| "-".to_string(), |x| format!("`{}`", x)),
                config.description.replace('\n', "<br>"),
                config
                    .default_value
                    .map(|value| format!("`{}`", value))
                    .unwrap_or("-".to_string()),
                config
                    .example
                    .map(|value| value.to_owned())
                    .unwrap_or("-".to_string()),
                String::from(if config.is_mandatory {
                    ":heavy_check_mark:"
                } else {
                    "-"
                }),
            ]
        })
        .collect::<Vec<Vec<String>>>();
    markdown::format_table(
        &[
            "Parameter",
            "Command line (long)",
            ":Command line (short):",
            "Environment variable",
            "Description",
            "Default value",
            "Example",
            ":Mandatory:",
        ],
        &subcommands_lines,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{Args, CommandFactory, Parser, Subcommand};
    use regex::Regex;

    #[derive(Args, Clone, Debug)]
    struct StructSubCommandB {
        /// The path of SubCommandB
        path: String,
    }

    #[derive(Subcommand, Debug, Clone)]
    enum MySubCommands {
        /// Help for Subcommand A.
        SubCommandA {
            /// First param.
            #[clap(long)]
            param_of_a: bool,
        },
        /// Help for Subcommand B.
        SubCommandB(StructSubCommandB),
    }

    #[derive(Parser, Debug, Clone)]
    #[command(version)]
    pub struct MyCommand {
        /// Available commands
        #[clap(subcommand)]
        command: MySubCommands,

        /// Run Mode
        #[clap(short, long, default_value = "dev")]
        run_mode: String,

        #[clap()]
        param_without_default: String,

        #[clap(long, env = "ENV_VARIABLE")]
        from_env: String,
    }

    #[derive(Parser, Debug, Clone)]
    pub struct MyCommandWithOnlySubCommand {}

    #[test]
    fn test_format_arg_without_struct_doc() {
        let mut command = MyCommand::command();
        let doc = doc_markdown_with_config(&mut command, None);

        assert!(
            doc.contains("| `run_mode` | `--run-mode` | `-r` | - | Run Mode | `dev` | - | - |"),
            "Generated doc: {doc}"
        );
        assert!(
            doc.contains(
                "| `param_without_default` | - | - | - | - | - | - | :heavy_check_mark: |"
            ),
            "Generated doc: {doc}"
        );
    }

    #[test]
    fn test_format_parameter_with_env_variable() {
        let mut command = MyCommand::command();
        let doc = doc_markdown_with_config(&mut command, None);

        assert!(
            doc.contains("| `from_env` | `--from-env` | - | `ENV_VARIABLE` | - | - | - | :heavy_check_mark: |"),
            "Generated doc: {doc}"
        );
    }

    #[test]
    fn test_format_arg_with_empty_struct_doc() {
        let mut command = MyCommand::command();
        let merged_struct_doc = StructDoc::new();
        let doc = doc_markdown_with_config(&mut command, Some(&merged_struct_doc));

        assert!(
            doc.contains("| `run_mode` | `--run-mode` | `-r` | - | Run Mode | `dev` | - | - |"),
            "Generated doc: {doc}"
        );
        assert!(
            doc.contains(
                "| `param_without_default` | - | - | - | - | - | - | :heavy_check_mark: |"
            ),
            "Generated doc: {doc}"
        );
    }

    #[test]
    fn test_format_subcommand_inlined() {
        let mut command = MyCommand::command();
        let doc = doc_markdown_with_config(&mut command, None);

        assert!(
            doc.contains("###  mithril-doc sub-command-a"),
            "Generated doc: {doc}"
        );
        // In `Commands:` part.
        assert!(
            doc.contains("sub-command-a  Help for Subcommand A"),
            "Generated doc: {doc}"
        );

        // In `Subcommand` table
        assert!(
            doc.contains("| **sub-command-a** | Help for Subcommand A |"),
            "Generated doc: {doc}"
        );
    }

    #[test]
    fn test_format_subcommand_on_separate_struct() {
        let mut command = MyCommand::command();
        let doc = doc_markdown_with_config(&mut command, None);

        assert!(
            doc.contains("###  mithril-doc sub-command-b"),
            "Generated doc: {doc}"
        );
        // In `Commands:` part.
        assert!(
            doc.contains("sub-command-b  Help for Subcommand B"),
            "Generated doc: {doc}"
        );

        // In `Subcommand` table
        assert!(
            doc.contains("| **sub-command-b** | Help for Subcommand B |"),
            "Generated doc: {doc}"
        );
        assert!(
            doc.contains("| `path` | - | - | - | The path of SubCommandB"),
            "Generated doc: {doc}"
        );

        assert!(
            Regex::new(r"Arguments:\s+<PATH>\s+The path of SubCommandB")
                .unwrap()
                .is_match(&doc),
            "Generated doc: {doc}"
        );
    }

    #[test]
    fn test_should_not_create_chapter_for_subcommand_help() {
        let mut command = MyCommand::command();
        let doc = doc_markdown_with_config(&mut command, None);

        assert!(
            doc.contains("###  mithril-doc sub-command-b"),
            "Generated doc: {doc}"
        );
        assert!(
            !doc.contains("###  mithril-doc help"),
            "Generated doc: {doc}"
        );
    }
    #[test]
    fn test_should_not_display_parameter_table_when_only_help_argument() {
        {
            let mut command = MyCommand::command();
            let doc = doc_markdown_with_config(&mut command, None);
            assert!(
                doc.contains("| `help` | `--help` | `-h` |"),
                "Generated doc: {doc}"
            );
        }
        {
            let mut command = MyCommandWithOnlySubCommand::command();
            let doc = doc_markdown_with_config(&mut command, None);
            assert!(
                !doc.contains("| `help` | `--help` | `-h` |"),
                "Generated doc: {doc}"
            );
        }
    }

    #[test]
    fn test_doc_markdown_include_config_parameters() {
        {
            let mut command = MyCommand::command();
            let doc = doc_markdown_with_config(&mut command, None);

            assert!(
                !doc.contains("| Param A from config |"),
                "Generated doc: {doc}"
            );
            assert!(
                !doc.contains("| `ConfigA` | - | - |"),
                "Generated doc: {doc}"
            );
        }
        {
            let struct_doc = {
                let mut s = StructDoc::default();
                s.add_param(
                    "ConfigA",
                    "Param A from config",
                    Some("CONFIGA".to_string()),
                    Some("default config A".to_string()),
                    None,
                );
                s.add_param("ConfigB", "Param B from config", None, None, None);
                s
            };

            let mut command = MyCommand::command();
            let doc = doc_markdown_with_config(&mut command, Some(&struct_doc));

            assert!(
                doc.contains("| Param A from config |"),
                "Generated doc: {doc}"
            );
            assert!(
                doc.contains(
                    "| `ConfigA` | - | - | `CONFIGA` | Param A from config | `default config A` |"
                ),
                "Generated doc: {doc}"
            );
        }
    }
}
