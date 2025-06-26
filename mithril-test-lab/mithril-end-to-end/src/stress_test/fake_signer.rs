use async_recursion::async_recursion;
use indicatif::{ProgressBar, ProgressDrawTarget};
use reqwest::{RequestBuilder, StatusCode};
use slog_scope::{debug, info, warn};
use std::time::Duration;
use thiserror::Error;
use tokio::task::JoinSet;

use mithril_common::{
    entities::{Epoch, PartyId, Signer},
    messages::RegisterSignatureMessageHttp,
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

#[async_recursion]
async fn send_signer_registration_request(
    party_id: PartyId,
    http_request: RequestBuilder,
    try_register_until_registration_round_is_open: bool,
) -> StdResult<()> {
    let response = http_request.try_clone().unwrap().send().await.unwrap();

    match response.status() {
        StatusCode::CREATED => Ok(()),
        status if status.as_u16() == 550 && try_register_until_registration_round_is_open => {
            let error_message = response.text().await.unwrap();
            debug!(
                "Error 550 (REGISTRATION_ROUND_NOT_YET_OPENED) for First signer registration, waiting 250ms before trying again";
                "error_message" => &error_message
            );
            tokio::time::sleep(Duration::from_millis(250)).await;
            send_signer_registration_request(party_id, http_request, true).await
        }
        status => Err(LoadError::SignerRegistrationError {
            expected_http_code: 201,
            got_http_code: status.as_u16() as u32,
            party_id,
            error_message: response.text().await.unwrap(),
        }
        .into()),
    }
}

pub async fn try_register_signer_until_registration_round_is_open(
    aggregator: &Aggregator,
    signer: &Signer,
    epoch: Epoch,
    timeout: Duration,
) -> StdResult<()> {
    let mut register_message =
        payload_builder::generate_register_signer_message(&[signer.clone()], epoch);
    let register_message = register_message.swap_remove(0);
    let party_id = register_message.party_id.clone();

    let http_request = reqwest::Client::new()
        .post(format!("{}/register-signer", aggregator.endpoint()))
        .json(&register_message);

    spin_while_waiting!(
        {
            match send_signer_registration_request(register_message.party_id, http_request, true)
                .await
            {
                Ok(_) => {
                    info!("Signer registration succeeded, sending the other registrations...");
                }
                Err(err) => {
                    warn!("Signer Registration error for first signer caught, other registrations will be skipped: {err:?}");
                }
            }

            Ok(())
        },
        timeout,
        format!(
            "Trying to register signer '{party_id}' until the registration round is opened ...",
        ),
        format!("Aggregator failed to register signer '{party_id}' after {timeout:?}'",)
    )
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
    let register_url = format!("{}/register-signer", aggregator.endpoint());
    let mut errors = 0;

    for register in register_messages {
        let http_request = http_client.post(&register_url).json(&register);

        join_set.spawn(async move {
            send_signer_registration_request(register.party_id, http_request, false).await
        });
    }

    while let Some(res) = join_set.join_next().await {
        let res = res.expect("Tokio task join failed!");
        progress_bar.inc(1);

        if res.is_err() {
            warn!("Signer Registration error caught: {res:?}");
            errors += 1;
        }
    }

    progress_bar.finish();

    Ok(errors)
}

pub async fn register_signatures_to_aggregator(
    aggregator: &Aggregator,
    register_messages: Vec<RegisterSignatureMessageHttp>,
) -> StdResult<usize> {
    let mut join_set: JoinSet<StdResult<()>> = JoinSet::new();
    let progress_bar = ProgressBar::with_draw_target(
        Some(register_messages.len() as u64),
        ProgressDrawTarget::stdout(),
    );

    let http_client = reqwest::Client::new();

    for register in register_messages {
        let endpoint = aggregator.endpoint();
        let http_request = http_client
            .post(format!("{endpoint}/register-signatures"))
            .json(&register);

        join_set.spawn(async move {
            let response = http_request.send().await.unwrap();

            match response.status() {
                StatusCode::CREATED => Ok(()),
                // Signature buffered
                StatusCode::ACCEPTED => Ok(()),
                // Certificate already certified
                StatusCode::GONE => Ok(()),
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

    progress_bar.finish();

    Ok(errors)
}
