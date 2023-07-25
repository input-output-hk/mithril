use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use anyhow::Context;
use async_recursion::async_recursion;
use clap::Parser;

use indicatif::{ProgressBar, ProgressDrawTarget};
use mithril_common::{
    digesters::DummyImmutablesDbBuilder,
    entities::{
        Epoch, PartyId, ProtocolMessage, ProtocolParameters, SignedEntityType, SingleSignatures,
    },
    messages::{
        CertificateListItemMessage, EpochSettingsMessage, MithrilStakeDistributionListItemMessage,
        RegisterSignatureMessage, RegisterSignerMessage,
    },
    test_utils::{MithrilFixture, MithrilFixtureBuilder},
    StdResult,
};

use mithril_end_to_end::{Aggregator, BftNode};
use reqwest::StatusCode;
use serde::Deserialize;
use slog::Level;
use slog_scope::{info, warn};
use thiserror::Error;
use tokio::{select, task::JoinSet, time::sleep};

macro_rules! spin_while_waiting {
    ($block:block, $timeout:expr, $wait_message:expr, $timeout_message:expr) => {{
        let progress_bar = ProgressBar::new_spinner().with_message($wait_message);

        let spinner = async move {
            loop {
                progress_bar.tick();
                sleep(Duration::from_millis(50)).await;
            }
        };
        let probe = async move { $block };

        select! {
        _ = spinner => Err(String::new().into()),
        _ = sleep($timeout) => Err($timeout_message.into()),
        res = probe => res
        }
    }};
}

#[derive(Debug, Error)]
pub enum LoadError {
    #[error("Registering signer party_id={party_id}, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SignerRegistrationError {
        party_id: PartyId,
        expected_http_code: u32,
        got_http_code: u32,
        error_message: String,
    },
    #[error("Registering signatures for party_id={party_id}, expected HTTP code {expected_http_code} got {got_http_code} with the message: {error_message}.")]
    SignaturesRegistrationError {
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
pub fn generate_signer_data(
    number_of_signers: usize,
    protocol_parameters: ProtocolParameters,
) -> MithrilFixture {
    MithrilFixtureBuilder::default()
        .with_signers(number_of_signers)
        .with_protocol_parameters(protocol_parameters)
        .build()
}

/// Generate signer registration message
pub fn generate_register_signer_message(
    signers_fixture: &MithrilFixture,
    epoch: Epoch,
) -> Vec<RegisterSignerMessage> {
    signers_fixture
        .signers()
        .into_iter()
        .map(|signer| RegisterSignerMessage {
            epoch: Some(epoch),
            party_id: signer.party_id,
            verification_key: signer.verification_key.to_json_hex().unwrap(),
            verification_key_signature: signer.verification_key_signature,
            operational_certificate: signer.operational_certificate,
            kes_period: signer.kes_period,
        })
        .collect::<Vec<_>>()
}

/// Generate register signature message
pub fn generate_register_signature_message(
    signatures: &[SingleSignatures],
    signed_entity_type: SignedEntityType,
) -> Vec<RegisterSignatureMessage> {
    signatures
        .iter()
        .map(|s| RegisterSignatureMessage {
            signed_entity_type: Some(signed_entity_type.clone()),
            party_id: s.party_id.clone(),
            signature: s.signature.clone().to_json_hex().unwrap(),
            won_indexes: s.won_indexes.clone(),
        })
        .collect::<Vec<_>>()
}

/// Wait for http response until timeout
pub async fn wait_for_http_response(url: &str, timeout: Duration, message: &str) -> StdResult<()> {
    spin_while_waiting!(
        {
            while reqwest::get(url).await.is_err() {
                sleep(Duration::from_millis(300)).await;
            }
            Ok(())
        },
        timeout,
        message.to_owned(),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for a given epoch in the epoch settings until timeout
pub async fn wait_for_epoch_settings_at_epoch(
    aggregator: &Aggregator,
    timeout: Duration,
    epoch: Epoch,
) -> StdResult<()> {
    let url = &format!("{}/epoch-settings", aggregator.endpoint());
    spin_while_waiting!(
        {
            while let Ok(response) = reqwest::get(url).await {
                match response.status() {
                    StatusCode::OK => {
                        let epoch_settings = response.json::<EpochSettingsMessage>().await.unwrap();

                        if epoch_settings.epoch >= epoch {
                            break;
                        }
                        sleep(Duration::from_millis(300)).await
                    }
                    s if s.is_server_error() => {
                        warn!(
                            "Server error while waiting for the Aggregator, http code: {}",
                            s
                        );
                        break;
                    }
                    _ => sleep(Duration::from_millis(300)).await,
                }
            }
            Ok(())
        },
        timeout,
        format!("Waiting for epoch {epoch}"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for pending certificate
pub async fn wait_for_pending_certificate(
    aggregator: &Aggregator,
    timeout: Duration,
) -> StdResult<()> {
    let url = &format!("{}/certificate-pending", aggregator.endpoint());
    spin_while_waiting!(
        {
            while let Ok(response) = reqwest::get(url).await {
                match response.status() {
                    StatusCode::OK => {
                        break;
                    }
                    s if s.is_server_error() => {
                        warn!(
                            "Server error while waiting for the Aggregator, http code: {}",
                            s
                        );
                        break;
                    }
                    _ => sleep(Duration::from_millis(300)).await,
                }
            }
            Ok(())
        },
        timeout,
        format!("Waiting for pending certificate"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

#[async_recursion]
async fn request_first_list_item<I>(url: &str) -> Result<I, String>
where
    for<'a> I: Deserialize<'a> + Sync + Send + Clone,
{
    sleep(Duration::from_millis(300)).await;

    match reqwest::get(url).await {
        Ok(response) => match response.status() {
            StatusCode::OK => match response.json::<Vec<I>>().await.as_deref() {
                Ok([first_item, ..]) => Ok(first_item.to_owned()),
                Ok(&[]) => request_first_list_item::<I>(url).await,
                Err(err) => Err(format!("Invalid list body : {err}")),
            },
            s if s.is_server_error() => {
                let message = format!(
                    "Server error while waiting for the Aggregator, http code: {}",
                    s
                );
                warn!("{message}");
                Err(message)
            }
            _ => request_first_list_item::<I>(url).await,
        },
        Err(err) => Err(format!("Request to `{url}` failed: {err}")),
    }
}

/// Wait for certificates
pub async fn wait_for_certificates(
    aggregator: &Aggregator,
    timeout: Duration,
) -> StdResult<CertificateListItemMessage> {
    let url = &format!("{}/certificates", aggregator.endpoint());
    spin_while_waiting!(
        {
            request_first_list_item::<CertificateListItemMessage>(url)
                .await
                .map_err(|e| e.into())
        },
        timeout,
        format!("Waiting for certificates"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

/// Wait for Mithril Stake Distribution artifacts
pub async fn wait_for_mithril_stake_distribution_artifacts(
    aggregator: &Aggregator,
    timeout: Duration,
) -> StdResult<MithrilStakeDistributionListItemMessage> {
    let url = &format!(
        "{}/artifact/mithril-stake-distributions",
        aggregator.endpoint()
    );
    spin_while_waiting!(
        {
            request_first_list_item::<MithrilStakeDistributionListItemMessage>(url)
                .await
                .map_err(|e| e.into())
        },
        timeout,
        format!("Waiting for mithril stake distribution artifacts"),
        format!("Aggregator did not get a response after {timeout:?} from '{url}'")
    )
}

pub async fn register_signers_to_aggregator(
    aggregator: &Aggregator,
    signers_fixture: &MithrilFixture,
    epoch: Epoch,
) -> StdResult<usize> {
    let register_messages = generate_register_signer_message(signers_fixture, epoch);

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar = ProgressBar::with_draw_target(
        Some(register_messages.len() as u64),
        ProgressDrawTarget::stdout(),
    );
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
            warn!("Signer Registration error caught: {res:?}");
            errors += 1;
        }
    }

    Ok(errors)
}

pub async fn register_signatures_to_aggregator(
    aggregator: &Aggregator,
    signatures: &[SingleSignatures],
    signed_entity_type: SignedEntityType,
) -> StdResult<usize> {
    let register_messages = generate_register_signature_message(signatures, signed_entity_type);

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar = ProgressBar::with_draw_target(
        Some(register_messages.len() as u64),
        ProgressDrawTarget::stdout(),
    );
    for register in register_messages {
        let endpoint = aggregator.endpoint();
        join_set.spawn(async move {
            let response = reqwest::Client::new()
                .post(format!("{}/register-signatures", endpoint))
                .json(&register)
                .send()
                .await
                .unwrap();

            match response.status() {
                StatusCode::CREATED => Ok(()),
                status => Err(LoadError::SignaturesRegistrationError {
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
            warn!("Signer Signature Registration error caught: {res:?}");
            errors += 1;
        }
    }

    Ok(errors)
}

pub fn write_stake_distribution(
    mock_stake_distribution_file_path: &Path,
    signers_fixture: &MithrilFixture,
) {
    let mock_stake_distribution_file = File::create(mock_stake_distribution_file_path).unwrap();
    serde_json::to_writer(
        &mock_stake_distribution_file,
        &signers_fixture.cardano_cli_stake_distribution(),
    )
    .expect("Writing the stake distribution into a file for the mock cardano cli failed");
}

pub fn write_epoch(mock_epoch_file_path: &Path, epoch: Epoch) {
    let mock_epoch_file = File::create(mock_epoch_file_path).unwrap();
    write!(&mock_epoch_file, "{}", *epoch)
        .expect("Writing the epoch into a file for the mock cardano cli failed");
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
    #[arg(long, default_value = "20")]
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
    let mock_stake_distribution_file_path = args.work_dir.join("stake_distribution.json");
    let mock_epoch_file_path = args.work_dir.join("epoch.txt");
    let mut current_epoch = Epoch(1);
    info!(">> Starting stress test with options: {opts:?}");

    info!(">> Creation of the Signer Key Registrations payloads");
    let signers_fixture =
        generate_signer_data(opts.num_signers, ProtocolParameters::new(5, 100, 0.65));

    info!(">> Precompute message & signatures for MithrilStakeDistribution signed entity");
    let mithril_stake_distribution_message = {
        let mut message = ProtocolMessage::new();
        message.set_message_part(
            mithril_common::entities::ProtocolMessagePartKey::NextAggregateVerificationKey,
            signers_fixture.compute_and_encode_avk(),
        );

        message
    };
    let mithril_stake_distribution_signatures: Vec<SingleSignatures> = signers_fixture
        .signers_fixture()
        .iter()
        // filter map to exclude signers that could not sign because they lost the lottery
        .filter_map(|s| s.sign(&mithril_stake_distribution_message))
        .collect();

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

    write_epoch(&mock_epoch_file_path, current_epoch);
    write_stake_distribution(&mock_stake_distribution_file_path, &signers_fixture);

    aggregator.change_run_interval(Duration::from_secs(6));
    aggregator
        .set_mock_cardano_cli_file_path(&mock_stake_distribution_file_path, &mock_epoch_file_path);
    aggregator.set_protocol_parameters(&signers_fixture.protocol_parameters());
    aggregator.serve().unwrap();

    wait_for_http_response(
        &format!("{}/epoch-settings", aggregator.endpoint()),
        Duration::from_secs(10),
        "Waiting for the aggregator to start",
    )
    .await?;

    info!(">> Send the Signer Key Registrations payloads");
    let errors =
        register_signers_to_aggregator(&aggregator, &signers_fixture, current_epoch + 1).await?;
    assert_eq!(0, errors);

    info!(">> Move one epoch forward in order to issue the genesis certificate");
    current_epoch += 1;
    write_epoch(&mock_epoch_file_path, current_epoch);
    wait_for_epoch_settings_at_epoch(&aggregator, Duration::from_secs(10), current_epoch).await?;
    {
        info!(">> Compute genesis certificate");
        let mut genesis_aggregator = Aggregator::copy_configuration(&aggregator);
        genesis_aggregator
            .bootstrap_genesis()
            .await
            .expect("Genesis aggregator should be able to bootstrap genesis");

        sleep(Duration::from_secs(10)).await;
    }

    info!(">> Send the Signer Key Registrations payloads");
    let errors =
        register_signers_to_aggregator(&aggregator, &signers_fixture, current_epoch + 1).await?;
    assert_eq!(0, errors);

    info!(">> Move one epoch forward in order to start creating certificates");
    current_epoch += 1;
    write_epoch(&mock_epoch_file_path, current_epoch);
    wait_for_epoch_settings_at_epoch(&aggregator, Duration::from_secs(10), current_epoch).await?;

    info!(">> Send the Signer Key Registrations payloads");
    let errors =
        register_signers_to_aggregator(&aggregator, &signers_fixture, current_epoch + 1).await?;
    assert_eq!(0, errors);

    info!(">> Wait for pending certificate to be available");
    wait_for_pending_certificate(&aggregator, Duration::from_secs(30)).await?;

    info!(
        ">> Send the Signer Signatures Registrations payloads for MithrilStakeDistribution({:?})",
        current_epoch
    );
    let errors = register_signatures_to_aggregator(
        &aggregator,
        &mithril_stake_distribution_signatures,
        SignedEntityType::MithrilStakeDistribution(current_epoch),
    )
    .await?;
    assert_eq!(0, errors);

    info!(">> Wait for certificates to be available...");
    wait_for_certificates(&aggregator, Duration::from_secs(30)).await?;

    info!(">> Wait for artifacts to be available...");
    wait_for_mithril_stake_distribution_artifacts(&aggregator, Duration::from_secs(30)).await?;

    info!(">> All steps executed successfully, stopping all tasks...");

    aggregator.stop().await.unwrap();
    Ok(())
}
