use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, Table};
use slog_scope::debug;
use std::{collections::HashMap, sync::Arc};

use mithril_client::{
    common::TransactionHash, CardanoTransactionsProofs, MessageBuilder, MithrilCertificate,
    MithrilResult, VerifiedCardanoTransactions, VerifyCardanoTransactionsProofsError,
};

use crate::utils::{IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter};
use crate::{
    commands::{client_builder, SharedArgs},
    configuration::{ConfigError, ConfigSource},
    CommandContext,
};

/// Clap command to show a given Cardano transaction sets
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionsCertifyCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Genesis Verification Key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,

    /// Hashes of the transactions to certify.
    #[clap(value_delimiter = ',', required = true)]
    transactions_hashes: Vec<String>,
}

impl CardanoTransactionsCertifyCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Cardano transaction certify command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;

        let progress_output_type = if self.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 4);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
            )))
            .build()?;

        progress_printer.report_step(1, "Fetching a proof for the given transactions…")?;
        let cardano_transaction_proof = client
            .cardano_transaction()
            .get_proofs(&self.transactions_hashes)
            .await
            .with_context(|| {
                format!(
                    "Can not get proof from aggregator, transactions hashes: '{:?}'",
                    self.transactions_hashes
                )
            })?;
        debug!(
            "Got Proof from aggregator, proof: {:?}",
            cardano_transaction_proof
        );

        let verified_transactions =
            Self::verify_proof_validity(2, &progress_printer, &cardano_transaction_proof)?;

        progress_printer.report_step(
            3,
            "Fetching the associated certificate and verifying the certificate chain…",
        )?;
        let certificate = client
            .certificate()
            .verify_chain(&cardano_transaction_proof.certificate_hash)
            .await
            .with_context(|| {
                format!(
                    "Can not verify the certificate chain from certificate_hash: '{}'",
                    verified_transactions.certificate_hash()
                )
            })?;

        Self::verify_proof_match_certificate(
            4,
            &progress_printer,
            &certificate,
            &verified_transactions,
        )?;

        Self::log_certify_information(
            &verified_transactions,
            &cardano_transaction_proof.non_certified_transactions,
            self.is_json_output_enabled(),
        )
    }

    fn verify_proof_validity(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        cardano_transaction_proof: &CardanoTransactionsProofs,
    ) -> MithrilResult<VerifiedCardanoTransactions> {
        progress_printer.report_step(step_number, "Verifying the proof…")?;
        match cardano_transaction_proof.verify() {
            Ok(verified_transactions) => Ok(verified_transactions),
            Err(VerifyCardanoTransactionsProofsError::NoCertifiedTransaction) => Err(anyhow!(
                "Mithril could not certify any of the given transactions.

Mithril may not have signed those transactions yet, please try again later."
            )),
            err => err.with_context(|| "Proof verification failed"),
        }
    }

    fn verify_proof_match_certificate(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        verified_transactions: &VerifiedCardanoTransactions,
    ) -> MithrilResult<()> {
        progress_printer.report_step(
            step_number,
            "Verify that the proof is signed in the associated certificate",
        )?;
        let message = MessageBuilder::new()
            .compute_cardano_transactions_proofs_message(certificate, verified_transactions);
        if !certificate.match_message(&message) {
            return Err(anyhow!(
                "Proof and certificate don't match (certificate hash = '{}').",
                certificate.hash
            ));
        }

        Ok(())
    }

    fn log_certify_information(
        verified_transactions: &VerifiedCardanoTransactions,
        non_certified_transactions: &[TransactionHash],
        json_output: bool,
    ) -> MithrilResult<()> {
        if json_output {
            println!(
                r#"{{"certified_transactions": {}, "non_certified_transactions": {}}}"#,
                serde_json::to_string(verified_transactions.certified_transactions())?,
                serde_json::to_string(non_certified_transactions)?,
            );
        } else {
            println!(
                r###"Cardano transactions proof has been successfully signed in the associated Mithril certificate."###,
            );

            if !non_certified_transactions.is_empty() {
                println!(
                    r###"
No proof could be computed for some Cardano transactions. Mithril may not have signed those transactions yet, please try again later."###,
                );
            }

            let result_table = verified_transactions
                .certified_transactions()
                .iter()
                .map(|tx| {
                    vec![
                        tx.cell(),
                        "✅".cell().justify(cli_table::format::Justify::Center),
                    ]
                })
                .chain(non_certified_transactions.iter().map(|tx| {
                    vec![
                        tx.cell(),
                        "❌".cell().justify(cli_table::format::Justify::Center),
                    ]
                }))
                .table()
                .title(vec!["Transaction Hash", "Certified"]);

            print_stdout(result_table)?
        }

        Ok(())
    }
}

impl ConfigSource for CardanoTransactionsCertifyCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(genesis_verification_key) = self.genesis_verification_key.clone() {
            map.insert(
                "genesis_verification_key".to_string(),
                genesis_verification_key,
            );
        }

        Ok(map)
    }
}
