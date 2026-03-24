use anyhow::{Context, anyhow};
use cli_table::{Cell, Table, print_stdout};
use mithril_client::{
    CardanoTransactionsProofsV2, MessageBuilder, MithrilCertificate, MithrilResult,
    VerifiedCardanoTransactionsV2, VerifyProofsV2Error, common::TransactionHash,
};
use slog::debug;

use crate::utils::ProgressPrinter;

pub async fn execute(
    transactions_hashes: &Vec<String>,
    client: mithril_client::Client,
    progress_printer: ProgressPrinter,
    is_json_output_enabled: bool,
    logger: slog::Logger,
) -> MithrilResult<()> {
    progress_printer.report_step(1, "Fetching a proof for the given transactions…")?;
    let cardano_transaction_proof = client
        .cardano_transaction_v2()
        .get_proof(transactions_hashes)
        .await
        .with_context(|| {
            format!(
                "Can not get proof from aggregator, transactions hashes: '{:?}'",
                transactions_hashes
            )
        })?;
    debug!(logger, "Got Proof from aggregator"; "proof" => ?cardano_transaction_proof);

    let verified_transactions =
        verify_proof_validity(2, &progress_printer, &cardano_transaction_proof)?;

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

    verify_proof_match_certificate(4, &progress_printer, &certificate, &verified_transactions)?;

    log_certify_information(
        &verified_transactions,
        &cardano_transaction_proof.non_certified_transactions,
        is_json_output_enabled,
    )
}

fn verify_proof_validity(
    step_number: u16,
    progress_printer: &ProgressPrinter,
    cardano_transaction_proof: &CardanoTransactionsProofsV2,
) -> MithrilResult<VerifiedCardanoTransactionsV2> {
    progress_printer.report_step(step_number, "Verifying the proof…")?;
    match cardano_transaction_proof.verify() {
        Ok(verified_transactions) => Ok(verified_transactions),
        Err(VerifyProofsV2Error::NoCertifiedItem(..)) => Err(anyhow!(
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
    verified_transactions: &VerifiedCardanoTransactionsV2,
) -> MithrilResult<()> {
    progress_printer.report_step(
        step_number,
        "Verify that the proof is signed in the associated certificate",
    )?;
    let message = MessageBuilder::new()
        .compute_cardano_transactions_proofs_v2_message(certificate, verified_transactions);
    if !certificate.match_message(&message) {
        return Err(anyhow!(
            "Proof and certificate don't match (certificate hash = '{}').",
            certificate.hash
        ));
    }

    Ok(())
}

fn log_certify_information(
    verified_transactions: &VerifiedCardanoTransactionsV2,
    non_certified_transactions: &[TransactionHash],
    json_output: bool,
) -> MithrilResult<()> {
    if json_output {
        println!(
            r#"{{"certified_transactions": {}, "non_certified_transactions": {}}}"#,
            serde_json::to_string(
                &verified_transactions
                    .certified_transactions()
                    .iter()
                    .map(|tx| {
                        serde_json::json!({
                            "transaction_hash": tx.transaction_hash,
                            "block_hash": tx.block_hash,
                            "block_number": tx.block_number,
                            "slot_number": tx.slot_number,
                        })
                    })
                    .collect::<Vec<_>>()
            )?,
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
                    tx.transaction_hash.clone().cell(),
                    tx.block_hash.clone().cell(),
                    format!("{}", tx.block_number).cell(),
                    format!("{}", tx.slot_number).cell(),
                    "✅".cell().justify(cli_table::format::Justify::Center),
                ]
            })
            .chain(non_certified_transactions.iter().map(|tx| {
                vec![
                    tx.cell(),
                    String::new().cell(),
                    String::new().cell(),
                    String::new().cell(),
                    "❌".cell().justify(cli_table::format::Justify::Center),
                ]
            }))
            .table()
            .title(vec![
                "Transaction Hash".cell(),
                "Block Hash".cell(),
                "Block Number".cell(),
                "Slot Number".cell(),
                "Certified".cell(),
            ]);

        print_stdout(result_table)?
    }

    Ok(())
}
