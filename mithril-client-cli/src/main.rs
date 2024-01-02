#![doc = include_str!("../README.md")]

use anyhow::Context;
use std::ffi::OsStr;
use clap::builder::StyledStr;
use clap::{Parser, Subcommand, CommandFactory, Command, Arg};
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog::{Drain, Fuse, Level, Logger};
use slog_async::Async;
use slog_scope::debug;
use slog_term::Decorator;
use std::io::Write;
use std::sync::Arc;
use std::{fs::File, path::PathBuf};

use mithril_client::MithrilResult;

use mithril_client_cli::commands::{
    cardano_transaction::CardanoTransactionCommands,
    mithril_stake_distribution::MithrilStakeDistributionCommands, snapshot::SnapshotCommands,
};

enum LogOutputType {
    Stdout,
    File(String),
}

impl LogOutputType {
    fn get_writer(&self) -> MithrilResult<Box<dyn Write + Send>> {
        let writer: Box<dyn Write + Send> = match self {
            LogOutputType::Stdout => Box::new(std::io::stdout()),
            LogOutputType::File(filepath) => Box::new(
                File::create(filepath)
                    .with_context(|| format!("Can not create output log file: {}", filepath))?,
            ),
        };

        Ok(writer)
    }
}

#[derive(Parser, Debug, Clone)]
#[clap(name = "mithril-client")]
#[clap(
    about = "This program shows, downloads and verifies certified blockchain artifacts.",
    long_about = None
)]
#[command(version)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: ArtifactCommands,

    /// Run Mode.
    #[clap(long, env = "RUN_MODE", default_value = "dev")]
    run_mode: String,

    /// Verbosity level (-v=warning, -vv=info, -vvv=debug).
    #[clap(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Directory where configuration file is located.
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,

    /// Override configuration Aggregator endpoint URL.
    #[clap(long, env = "AGGREGATOR_ENDPOINT")]
    aggregator_endpoint: Option<String>,

    /// Enable JSON output for logs displayed according to verbosity level
    #[clap(long)]
    log_format_json: bool,

    /// Redirect the logs to a file
    #[clap(long, alias("o"))]
    log_output: Option<String>,

    /// Enable unstable commands (Such as Cardano Transactions)
    #[clap(long)]
    unstable: bool,
}

pub mod markdown_formatter {
 
    pub fn format_table(header: &Vec<&str>, lines: &Vec<Vec<String>>) -> String {
        format!("{}\n{}",
            format_table_header(header),
            lines.iter().map(|line| format_table_line(line)).collect::<Vec<String>>().join("\n"),
        )
    }

    pub fn format_table_line(data: &Vec<String>) -> String {
        format!("| {} |", data.join(" | "))
    }

    pub fn format_table_header(data: &Vec<&str>) -> String {
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
}

impl Args {

    pub fn doc_markdown() -> String {
        // See: https://github1s.com/clap-rs/clap/blob/HEAD/clap_builder/src/builder/command.rs#L1989

        let mut cmd: clap::Command = <Self as CommandFactory>::command();

        fn format_arg(arg: &Arg) -> Vec<String> {
            let parameter = format!("`{}`", arg.get_id());
            let short_option = arg.get_short().map_or("".into(), |c| format!("`-{}`", c));
            let long_option = arg.get_long().map_or("".into(), |c| format!("`--{}`", c));
            let env_variable = arg.get_env().and_then(OsStr::to_str).map_or("".into(), |s| format!("`{}`", s));
            let description = String::from("?");
            let default_value = arg.get_default_values().iter().map(|s| format!("`{}`", s.to_string_lossy())).collect::<Vec<String>>().join(",");
            let example = arg.get_help().map_or("".into(), StyledStr::to_string);
            let is_required = String::from(if arg.is_required_set() {":heavy_check_mark:"} else {"-"});
            
            vec!(parameter, long_option, short_option, env_variable, description, default_value, example, is_required)
        }

        fn format_parameters(cmd: &Command) -> String {
            if cmd.get_arguments().peekable().peek().is_some() {

                let parameters_table = format!("Here is a list of the available parameters:\n### Configuration parameters\n\n{}\n",
                    markdown_formatter::format_table(
                        &vec!("Parameter", "Command line (long)", ":Command line (short):", "Environment variable", "Description", "Default value", "Example", ":Mandatory:"),
                        &cmd.get_arguments().map(format_arg).collect(),
                    ),
                );

                let parameters_explanation = format!("\n\
                    The configuration parameters can be set in either of the following ways:\n\
                    \n\
                    1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.\n\
                    \n\
                    2. The value can be overridden by an environment variable with the parameter name in uppercase.\n\
                    ");
                format!("{}\n{}", parameters_explanation, parameters_table)
            } else {
                String::from("")
            }
        }

        fn format_command(cmd: &mut Command, parent: Option<String>) -> String {
            let parent_ancestors = parent.clone().map_or("".into(), |s| format!("{} ", s));
            let title = format!("### {}{}\n", parent_ancestors, cmd.get_name());
            let description = format!("{}", cmd.get_about().map_or("".into(), StyledStr::to_string));

            let subcommands_table = if cmd.get_subcommands().peekable().peek().is_some() {
                let subcommands_lines = cmd.get_subcommands().map(|command| {
                    vec!(
                        format!("**{}**", command.get_name()),
                        command.get_all_aliases().collect::<Vec<&str>>().join(","),
                        command.get_about().map_or("".into(), StyledStr::to_string)
                    )
                }).collect();

                markdown_formatter::format_table(
                    &vec!("Subcommand", "Aliases", "Performed action"),
                    &subcommands_lines,
                )
            } else {
                String::from("")
            };

            let parameters = format_parameters(&cmd);

            let subcommands = cmd.get_subcommands().map(|sub_command: &Command| {
                format_command(&mut sub_command.clone(), Some(format!("{} {}", parent_ancestors, cmd.get_name())))
            }).collect::<Vec<String>>();
           
            // let usage = format!("```bash\n{}\n```", cmd.render_usage()); // Already in help 
            // let help = format!("```bash\n{}\n```", cmd.render_help());
            let help = format!("```bash\n{}\n```", cmd.render_long_help()); // More readable than help

            format!("{}\n{}\n{}\n{}\n{}\n{}", title, description, help, subcommands_table, parameters, subcommands.join("\n"))

        }

        format_command(&mut cmd, None)

    }

    pub fn document() {

        // See: https://github1s.com/clap-rs/clap/blob/HEAD/clap_builder/src/builder/command.rs#L1989

        let cmd: clap::Command = <Self as CommandFactory>::command();

        fn format_value_names(arg: &Arg) -> String {

            fn format_required(arg: &Arg, value: &str) -> String {
                if arg.is_required_set() {
                    format!("<{value}>")
                } else {
                    format!("[{value}]")
                }
            }

            arg.get_value_names().map(|values| {
                    values.iter().map(|value| format_required(arg, value)).collect::<Vec<String>>().join(" ")
                }
            ).unwrap_or(String::from("?"))
            
            //format!("{} (postional={}, required={})", formatted_names, arg.is_positional(), arg.is_required_set())
        }

        fn format_arg(arg: &Arg) -> String {
            format!("  {}{} {} {}",
                arg.get_short().map(|c| format!("-{}, ", c)).unwrap_or(String::from("")),
                arg.get_long().map(|c| format!("--{}", c)).unwrap_or(String::from("")),
                format_value_names(arg),
                arg.get_help().map(|s| s.to_string()).unwrap_or(String::from("")),
            )
        }

        fn format_command(command: &Command) -> String {
           // println!("POSITIONAL ARGUMENTS:");
           // for arg in cmd.get_positionals() {
           //     println!("{}", format_arg(arg));
           // }

            let positional_arguments = command.get_positionals().map(|arg| format!("{}",format_value_names(arg))).collect::<Vec<String>>();
            let sub_commands = command.get_subcommands().map(|cmd| format!("   {}",format_command(cmd))).collect::<Vec<String>>();
            let arguments = command.get_arguments().map(|arg| format!("   {}",format_arg(arg))).collect::<Vec<String>>();

            format!("{} {}\n{}\n{}",
                command.get_name(), 
                positional_arguments.join(" "), 
                arguments.join("\n"),
                sub_commands.join("\n"),
            )
        }


        println!("ARGUMENTS:");
        for arg in cmd.get_arguments() {
            println!("{}", format_arg(arg));
        }

        println!("SUBCOMMANDS:");
        for command in cmd.get_subcommands() {
            println!("{}", format_command(command));
        }

    }

    pub async fn execute(&self) -> MithrilResult<()> {
        debug!("Run Mode: {}", self.run_mode);
        let filename = format!("{}/{}.json", self.config_directory.display(), self.run_mode);
        debug!("Reading configuration file '{}'.", filename);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(config::File::with_name(&filename).required(false))
            .add_source(self.clone())
            .set_default("download_dir", "")?;

        self.command.execute(self.unstable, config).await
    }

    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn get_log_output_type(&self) -> LogOutputType {
        if let Some(output_filepath) = &self.log_output {
            LogOutputType::File(output_filepath.to_string())
        } else {
            LogOutputType::Stdout
        }
    }

    fn wrap_drain<D: Decorator + Send + 'static>(&self, decorator: D) -> Fuse<Async> {
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();

        slog_async::Async::new(drain).build().fuse()
    }

    fn build_logger(&self) -> MithrilResult<Logger> {
        let log_output_type = self.get_log_output_type();
        let writer = log_output_type.get_writer()?;

        let drain = if self.log_format_json {
            let drain = slog_bunyan::new(writer).set_pretty(false).build().fuse();
            let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();

            slog_async::Async::new(drain).build().fuse()
        } else {
            match log_output_type {
                LogOutputType::Stdout => self.wrap_drain(slog_term::TermDecorator::new().build()),
                LogOutputType::File(_) => self.wrap_drain(slog_term::PlainDecorator::new(writer)),
            }
        };

        Ok(Logger::root(Arc::new(drain), slog::o!()))
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut map = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(aggregator_endpoint) = self.aggregator_endpoint.clone() {
            map.insert(
                "aggregator_endpoint".to_string(),
                Value::new(Some(&namespace), ValueKind::from(aggregator_endpoint)),
            );
        }

        Ok(map)
    }
}

#[derive(Subcommand, Debug, Clone)]
enum ArtifactCommands {
    #[clap(subcommand)]
    Snapshot(SnapshotCommands),

    #[clap(subcommand, alias("msd"))]
    MithrilStakeDistribution(MithrilStakeDistributionCommands),

    #[clap(subcommand, alias("ctx"))]
    CardanoTransaction(CardanoTransactionCommands),
    
    #[clap(alias("doc"))]
    GenerateDoc(GenerateDocCommands),
}

impl ArtifactCommands {
    pub async fn execute(
        &self,
        unstable_enabled: bool,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> MithrilResult<()> {
        match self {
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
            Self::MithrilStakeDistribution(cmd) => cmd.execute(config_builder).await,
            Self::CardanoTransaction(ctx) => {
                if !unstable_enabled {
                    Err(anyhow::anyhow!(
                        "The \"cardano-transaction\" subcommand is only accepted using the \
                        --unstable flag.\n \
                    \n \
                    ie: \"mithril-client --unstable cardano-transaction list\""
                    ))
                } else {
                    ctx.execute(config_builder).await
                }
            },
            Self::GenerateDoc(cmd) => cmd.execute().await,
        }
    }
}
/// Generate documentation
#[derive(Parser, Debug, Clone)]
pub struct GenerateDocCommands {
    /// Generated documentation file 
    #[clap(long, default_value = "generated_doc.md")]
    output: String,
}
impl GenerateDocCommands {
    pub async fn execute(&self) -> MithrilResult<()> {
        let doc = Args::doc_markdown();
        let mut buffer: File = File::create(&self.output)?;
        buffer.write(b"Generated doc\n\n")?;
        buffer.write(doc.as_bytes())?;
        println!("Documentation generated in file `{}`", &self.output);
        Ok(())
    }
}


#[tokio::main]
async fn main() -> MithrilResult<()> {
    //Args::document();
    let doc = Args::doc_markdown();
    let mut buffer: File = File::create("generated_doc.md")?;
    buffer.write(b"Generated doc\n\n")?;
    buffer.write(doc.as_bytes())?;

    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger()?);

    #[cfg(feature = "bundle_openssl")]
    openssl_probe::init_ssl_cert_env_vars();

    args.execute().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn fail_if_cardano_tx_command_is_used_without_unstable_flag() {
        let args = Args::try_parse_from(["mithril-client", "cardano-transaction", "sets", "list"])
            .unwrap();

        args.execute()
            .await
            .expect_err("Should fail if unstable flag missing");
    }
}
