use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use chrono::Utc;
use clap::Parser;
use mithril_client::MithrilResult;

use crate::{
    CommandContext,
    commands::{
        cardano_db::{CardanoDbCommandsBackend, shared_steps},
        client_builder,
    },
    configuration::{ConfigError, ConfigSource},
    utils::{self, ExpanderUtils, IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter},
};

/// Clap command to verify a Cardano db and its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbVerifyCommand {
    ///Backend to use, either: `v1` (default, full database restoration only) or `v2` (full or partial database restoration)
    #[arg(short, long, value_enum, default_value_t = CardanoDbCommandsBackend::V2)]
    backend: CardanoDbCommandsBackend,

    /// Digest of the Cardano db snapshot to verify  or `latest` for the latest artifact
    ///
    /// Use the `list` command to get that information.
    digest: String,

    /// Directory from where the immutable will be verified.
    #[clap(long)]
    db_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoDbVerifyCommand {
    /// Main command execution
    pub async fn execute(&self, mut context: CommandContext) -> MithrilResult<()> {
        match self.backend {
            CardanoDbCommandsBackend::V1 => Err(anyhow::anyhow!(
                r#"The "verify" subcommand is not available for the v1, use --backend v2 instead"#,
            )),
            CardanoDbCommandsBackend::V2 => {
                context.config_parameters_mut().add_source(self)?;
                self.verify(&context).await
            }
        }
    }

    async fn verify(&self, context: &CommandContext) -> MithrilResult<()> {
        let db_dir = context.config_parameters().require("db_dir")?;
        let db_dir = Path::new(&db_dir);

        let progress_output_type = if context.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 4);
        let client = client_builder(context.config_parameters())?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
                context.logger().clone(),
            )))
            .with_logger(context.logger().clone())
            .build()?;

        client.cardano_database_v2().check_has_immutables(db_dir)?;

        let get_list_of_artifact_ids = || async {
            let cardano_db_snapshots = client.cardano_database_v2().list().await.with_context(
                || "Can not get the list of artifacts while retrieving the latest cardano db hash",
            )?;

            Ok(cardano_db_snapshots
                .iter()
                .map(|cardano_db| cardano_db.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database_v2()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .with_context(|| format!("Can not get the cardano db for hash: '{}'", self.digest))?;

        let certificate = shared_steps::fetch_certificate_and_verifying_chain(
            1,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        let immutable_file_range = shared_steps::immutable_file_range(None, None);

        let verified_digests = shared_steps::download_and_verify_digests(
            2,
            &progress_printer,
            &client,
            &certificate,
            &cardano_db_message,
        )
        .await?;

        let merkle_proof = shared_steps::compute_verify_merkle_proof(
            2,
            &progress_printer,
            &client,
            &certificate,
            &cardano_db_message,
            &immutable_file_range,
            db_dir,
        )
        .await?;

        let message = shared_steps::compute_cardano_db_snapshot_message(
            3,
            &progress_printer,
            &certificate,
            &merkle_proof,
        )
        .await?;

        shared_steps::verify_message_matches_certificate(
            &context.logger().clone(),
            4,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            db_dir,
        )
        .await?;

        Self::log_verified_information(
            db_dir,
            &cardano_db_message.hash,
            context.is_json_output_enabled(),
        )?;

        Ok(())
    }

    fn log_verified_information(
        db_dir: &Path,
        snapshot_hash: &str,
        json_output: bool,
    ) -> MithrilResult<()> {
        if json_output {
            let canonical_filepath = &db_dir.canonicalize().with_context(|| {
                format!("Could not get canonical filepath of '{}'", db_dir.display())
            })?;
            let json = serde_json::json!({
                "timestamp": Utc::now().to_rfc3339(),
                "verified_db_directory": canonical_filepath
            });
            println!("{json}");
        } else {
            println!(
                "Cardano database snapshot '{snapshot_hash}' archives have been successfully verified. Immutable files have been successfully verified with Mithril."
            );
        }
        Ok(())
    }
}

impl ConfigSource for CardanoDbVerifyCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(download_dir) = self.db_dir.clone() {
            let param = "db_dir".to_string();
            map.insert(
                param.clone(),
                utils::path_to_string(&download_dir)
                    .map_err(|e| ConfigError::Conversion(param, e))?,
            );
        }

        if let Some(genesis_verification_key) = self.genesis_verification_key.clone() {
            map.insert(
                "genesis_verification_key".to_string(),
                genesis_verification_key,
            );
        }

        Ok(map)
    }
}
