use std::{path::PathBuf, sync::Arc, time::Duration};

use anyhow::Context;
use clap::Parser;

use indicatif::{ProgressBar, ProgressDrawTarget};
use mithril_common::{
    digesters::DummyImmutablesDbBuilder,
    entities::{Epoch, PartyId, ProtocolParameters},
    messages::RegisterSignerMessage,
    test_utils::{MithrilFixture, MithrilFixtureBuilder},
    StdResult,
};

use mithril_end_to_end::{Aggregator, BftNode};
use reqwest::StatusCode;
use slog::Level;
use slog_scope::info;
use thiserror::Error;
use tokio::{select, task::JoinSet, time::sleep};

#[derive(Debug, Error)]
pub enum LoadError {
    #[error("Registering signer party_id={party_id}, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SignerRegistrationError {
        party_id: PartyId,
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
}

fn init_logger(opts: &MainOpts) -> slog_scope::GlobalLoggerGuard {
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let drain = slog::LevelFilter::new(drain, opts.log_level()).fuse();

    slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()))
}

/// Generate signer data
pub fn generate_signer_data(number_of_signers: usize) -> MithrilFixture {
    MithrilFixtureBuilder::default()
        .with_signers(number_of_signers)
        .build()
}

/// Generate signer registration
pub fn generate_register_message(signers_fixture: &MithrilFixture) -> Vec<RegisterSignerMessage> {
    let epoch = Epoch(2);
    signers_fixture
        .signers()
        .into_iter()
        .map(|signer| RegisterSignerMessage {
            epoch: Some(epoch),
            party_id: signer.party_id,
            verification_key: signer.verification_key,
            verification_key_signature: signer.verification_key_signature,
            operational_certificate: signer.operational_certificate,
            kes_period: signer.kes_period,
        })
        .collect::<Vec<_>>()
}

/// Wait for http response until timeout
pub async fn wait_for_http_response(url: &str, timeout: Duration, message: &str) -> StdResult<()> {
    let progress_bar = ProgressBar::new_spinner().with_message(message.to_owned());
    let spinner = async move {
        loop {
            progress_bar.tick();
            sleep(Duration::from_millis(50)).await;
        }
    };
    let probe = async move {
        while reqwest::get(url).await.is_err() {
            sleep(Duration::from_millis(300)).await;
        }
    };

    select! {
        _ = spinner => Err(String::new().into()),
        _ = sleep(timeout) => Err(format!("Aggregator did not get a response after {timeout:?} from '{url}'").into()),
        _ = probe => Ok(())
    }
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct MainOpts {
    /// Location of the Cardano CLI binary
    #[arg(short, long)]
    pub cardano_cli_path: PathBuf,

    /// Temporary location for logs, databases etc.
    #[arg(short, long)]
    pub temporary_path: Option<PathBuf>,

    /// Path of the Aggregator binary
    #[arg(short, long, default_value = "./target/debug")]
    pub aggregator_dir: PathBuf,

    /// Number of concurrent signers
    #[arg(long, default_value = "2")]
    pub num_signers: usize,

    /// Mithril technical Era
    #[arg(long, default_value = "thales")]
    pub mithril_era: String,

    /// Aggregator HTTP port
    #[arg(short = 'p', long, default_value = "8888")]
    server_port: u32,

    /// Log level
    #[arg(short='v', action = clap::ArgAction::Count)]
    verbose: u8,
}

impl MainOpts {
    /// get log level from parameters
    pub fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }
}

#[derive(Debug)]
pub struct AggregatorParameters {
    server_port: u32,
    bft_node: BftNode,
    cardano_cli_path: PathBuf,
    work_dir: PathBuf,
    bin_dir: PathBuf,
    mithril_era: String,
}

impl AggregatorParameters {
    fn new(opts: &MainOpts) -> StdResult<Self> {
        // configure a dummy immutable db
        let immutable_db = DummyImmutablesDbBuilder::new("load-tester")
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();

        let bft_node = BftNode {
            db_path: immutable_db.dir,
            socket_path: PathBuf::new(),
        };
        let tmp_dir = opts
            .temporary_path
            .as_ref()
            .cloned()
            .unwrap_or_else(|| std::env::temp_dir().join("load-aggregator"));

        if tmp_dir.exists() {
            std::fs::remove_dir_all(&tmp_dir).with_context(|| {
                format!(
                    "Could not remove existing temp directory '{}'.",
                    tmp_dir.display()
                )
            })?;
        }
        std::fs::create_dir_all(&tmp_dir)
            .with_context(|| format!("Could not create temp directory '{}'.", tmp_dir.display()))?;

        let cardano_cli_path = {
            if !opts.cardano_cli_path.exists() {
                Err(format!(
                    "Given cardano-cli path does not exist: {}",
                    opts.cardano_cli_path.display()
                ))?
            }

            opts.cardano_cli_path.canonicalize().with_context(|| {
                format!(
                    "Could not canonicalize path to the cardano-cli, path: {}",
                    opts.cardano_cli_path.display()
                )
            })?
        };

        let aggregator_parameters = AggregatorParameters {
            bft_node,
            bin_dir: opts.aggregator_dir.clone(),
            cardano_cli_path,
            server_port: opts.server_port,
            work_dir: tmp_dir,
            mithril_era: opts.mithril_era.clone(),
        };

        Ok(aggregator_parameters)
    }
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let opts = MainOpts::parse();
    let _logger = init_logger(&opts);
    let args = AggregatorParameters::new(&opts)?;
    info!(">> Starting stress test with options: {opts:?}");

    info!(">> Creation of the Signer Key Registrations payloads");
    let signers_fixture = generate_signer_data(opts.num_signers);
    let register_messages = generate_register_message(&signers_fixture);

    info!(">> Launch Aggregator");
    let mut aggregator = Aggregator::new(
        args.server_port as u64,
        &args.bft_node,
        &args.cardano_cli_path,
        &args.work_dir,
        &args.bin_dir,
        &args.mithril_era,
    )
    .unwrap();

    aggregator.set_protocol_parameters(&ProtocolParameters::default());
    aggregator.serve().unwrap();

    wait_for_http_response(
        &format!("{}/epoch-settings", aggregator.endpoint()),
        Duration::from_secs(10),
        "Waiting for the aggregator to start",
    )
    .await?;

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar =
        ProgressBar::with_draw_target(Some(opts.num_signers as u64), ProgressDrawTarget::stdout());

    info!(">> Send the Signer Key Registrations payloads");
    for register in register_messages {
        let endpoint = aggregator.endpoint();
        join_set.spawn(async move {
            let response = reqwest::Client::new()
                .post(format!("{}/register-signer", endpoint))
                .json(&register)
                .send()
                .await
                .unwrap();

            match response.status() {
                StatusCode::CREATED => Ok(()),
                status => Err(LoadError::SignerRegistrationError {
                    expected_http_code: 201,
                    got_http_code: status.as_u16() as u32,
                    party_id: register.party_id,
                    error_message: response.text().await.unwrap(),
                }
                .into()),
            }
        });
    }
    let mut errors = 0;

    while let Some(res) = join_set.join_next().await {
        let res = res.expect("Tokio task join failed!");
        progress_bar.inc(1);

        if res.is_err() {
            // eprintln!("Signer error caught: {res:?}");
            errors += 1;
        }
    }

    assert_eq!(opts.num_signers - 1, errors);

    // ensure POSTing payload gives 200
    aggregator.stop().await.unwrap();
    Ok(())
}
