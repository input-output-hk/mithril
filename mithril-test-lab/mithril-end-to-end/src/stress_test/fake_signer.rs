use indicatif::{ProgressBar, ProgressDrawTarget};
use reqwest::StatusCode;
use slog_scope::warn;
use thiserror::Error;
use tokio::task::JoinSet;

use mithril_common::{
    entities::{Epoch, PartyId, SignedEntityType, Signer, SingleSignatures},
    StdResult,
};

use crate::stress_test::payload_builder;
use crate::Aggregator;

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

pub async fn register_signers_to_aggregator(
    aggregator: &Aggregator,
    signers: &[Signer],
    epoch: Epoch,
) -> StdResult<usize> {
    let register_messages = payload_builder::generate_register_signer_message(signers, epoch);

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar = ProgressBar::with_draw_target(
        Some(register_messages.len() as u64),
        ProgressDrawTarget::stdout(),
    );

    let http_client = reqwest::Client::new();

    for register in register_messages {
        let endpoint = aggregator.endpoint();
        let http_request = http_client
            .post(format!("{}/register-signer", endpoint))
            .json(&register);

        join_set.spawn(async move {
            let response = http_request.send().await.unwrap();

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
    let register_messages =
        payload_builder::generate_register_signature_message(signatures, signed_entity_type);

    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar = ProgressBar::with_draw_target(
        Some(register_messages.len() as u64),
        ProgressDrawTarget::stdout(),
    );

    let http_client = reqwest::Client::new();

    for register in register_messages {
        let endpoint = aggregator.endpoint();
        let http_request = http_client
            .post(format!("{}/register-signatures", endpoint))
            .json(&register);

        join_set.spawn(async move {
            let response = http_request.send().await.unwrap();

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
