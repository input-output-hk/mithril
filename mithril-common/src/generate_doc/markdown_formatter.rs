use std::ffi::OsStr;

use clap::{Command, Arg, builder::StyledStr};



/// Format a list of label and a list of text list into a markdown table.
pub fn format_table(header: &[&str], lines: &[Vec<String>]) -> String {
    format!("{}\n{}",
        format_table_header(header),
        lines.iter().map(|line| format_table_line(line)).collect::<Vec<String>>().join("\n"),
    )
}

/// Format a list of text as a markdown table line.
pub fn format_table_line(data: &[String]) -> String {
    format!("| {} |", data.join(" | "))
}

/// Format a list of label to a markdown header.
/// To align the text to left, right or to center it, you need to ad `:` at left, right or both.
/// Example:  :Description: 
pub fn format_table_header(data: &[&str]) -> String {
    let headers = data.iter().map(|header| {
        let align_left = header.chars().next().map(|c| c == ':').unwrap_or(false);
        let align_right = header.chars().last().map(|c| c == ':').unwrap_or(false);
        let label = &header[(if align_left {1} else {0})..(header.len()-(if align_right {1} else {0}))];
        (label, align_left, align_right)
    }).collect::<Vec<(&str, bool, bool)>>();

    let sublines = headers.iter().map(|(label, left, right)| {
        format!("{}{}{}", if *left {":"} else {"-"}, "-".repeat(label.len()), if *right {":"} else {"-"})
    }).collect::<Vec<String>>();

    let labels = headers.iter().map(|(label, _, _)| {
        label.to_string()
    }).collect::<Vec<String>>();

    format!("| {} |\n|{}|", labels.join(" | "), sublines.join("|"))
}

/// Create a documentation of the command and its sub-commands. 
pub fn doc_markdown(cmd: &mut Command) -> String {
    // See: https://github1s.com/clap-rs/clap/blob/HEAD/clap_builder/src/builder/command.rs#L1989

    fn format_arg(arg: &Arg) -> Vec<String> {
        let parameter = format!("`{}`", arg.get_id());
        let short_option = arg.get_short().map_or("".into(), |c| format!("`-{}`", c));
        let long_option = arg.get_long().map_or("".into(), |c| format!("`--{}`", c));
        let env_variable = arg.get_env().and_then(OsStr::to_str).map_or("".into(), |s| format!("`{}`", s));
        let description = arg.get_help().map_or("".into(), StyledStr::to_string);
        let default_value = arg.get_default_values().iter().map(|s| format!("`{}`", s.to_string_lossy())).collect::<Vec<String>>().join(",");
        let example = String::from("?");
        let is_required = String::from(if arg.is_required_set() {":heavy_check_mark:"} else {"-"});
        
        vec!(parameter, long_option, short_option, env_variable, description, default_value, example, is_required)
    }

    fn format_parameters(cmd: &Command) -> String {
        if cmd.get_arguments().peekable().filter(|arg| arg.get_id().as_str() != "help").count() > 0 {
            let parameters_table = format!("Here is a list of the available parameters:\n### Configuration parameters\n\n{}\n",
                format_table(
                    &["Parameter", "Command line (long)", ":Command line (short):", "Environment variable", "Description", "Default value", "Example", ":Mandatory:"],
                    &cmd.get_arguments().map(format_arg).collect::<Vec<Vec<String>>>(),
                ),
            );

            let parameters_explanation = "\n\
                The configuration parameters can be set in either of the following ways:\n\
                \n\
                1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.\n\
                \n\
                2. The value can be overridden by an environment variable with the parameter name in uppercase.\n\
                ";
            format!("{}\n{}", parameters_explanation, parameters_table)
        } else {
            String::from("")
        }
    }

    fn format_command(cmd: &mut Command, parent: Option<String>) -> String {
        // It's important to start by calling `render_help` because that built the command.
        // The initialization add `help` command and default values for parameters.
        // This is why the command parameter is a mutable reference.

        // let usage = format!("```bash\n{}\n```", cmd.render_usage()); // Already in help 
        // let help = format!("```bash\n{}\n```", cmd.render_help());
        let help = format!("```bash\n{}\n```", cmd.render_long_help()); // More readable than help
        format_command_internal(cmd, parent, help)
    }

    fn format_command_internal(cmd: &Command, parent: Option<String>, help: String) -> String {
        let parent_ancestors = parent.clone().map_or("".into(), |s| format!("{} ", s));
        let title = format!("### {}{}\n", parent_ancestors, cmd.get_name());
        let description = cmd.get_about().map_or("".into(), StyledStr::to_string);

        let subcommands_table = if cmd.get_subcommands().peekable().peek().is_some() {
            let subcommands_lines = cmd.get_subcommands().map(|command| {
                vec!(
                    format!("**{}**", command.get_name()),
                    command.get_all_aliases().collect::<Vec<&str>>().join(","),
                    command.get_about().map_or("".into(), StyledStr::to_string)
                )
            }).collect::<Vec<Vec<String>>>();

            format_table(
                &["Subcommand", "Aliases", "Performed action"],
                &subcommands_lines,
            )
        } else {
            String::from("")
        };

        let parameters = format_parameters(cmd);

        let subcommands = cmd.get_subcommands()
            .filter(|sub_command| sub_command.get_name() != "help")
            .map(|sub_command: &Command| {
                format_command(&mut sub_command.clone(), Some(format!("{} {}", parent_ancestors, cmd.get_name())))
            }
        ).collect::<Vec<String>>();
            
        format!("{}\n{}\n{}\n{}\n{}\n{}", title, description, help, subcommands_table, parameters, subcommands.join("\n"))

    }

    format_command(cmd, None)

}