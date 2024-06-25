use serde::{Deserialize, Serialize};
use std::sync::Arc;
use warp::Filter;

use crate::http_server::routes::middlewares;
use crate::DependencyContainer;

#[derive(Deserialize, Serialize, Debug)]
struct CardanoTransactionProofQueryParams {
    transaction_hashes: String,
}

impl CardanoTransactionProofQueryParams {
    pub fn split_transactions_hashes(&self) -> Vec<String> {
        self.transaction_hashes
            .split(',')
            .map(|s| s.to_string())
            .collect()
    }

    pub fn sanitize(&self) -> Vec<String> {
        let mut transaction_hashes = self.split_transactions_hashes();
        transaction_hashes.sort();
        transaction_hashes.dedup();
        transaction_hashes
    }
}

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    proof_cardano_transaction(dependency_manager)
}

/// GET /proof/cardano-transaction
fn proof_cardano_transaction(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("proof" / "cardano-transaction")
        .and(warp::get())
        .and(warp::query::<CardanoTransactionProofQueryParams>())
        .and(middlewares::with_signed_entity_service(
            dependency_manager.clone(),
        ))
        .and(
            middlewares::validators::with_prover_transations_hash_validator(
                dependency_manager.clone(),
            ),
        )
        .and(middlewares::with_prover_service(dependency_manager))
        .and_then(handlers::proof_cardano_transaction)
}

mod handlers {
    use mithril_common::{
        entities::{CardanoTransactionsSnapshot, SignedEntity},
        messages::CardanoTransactionsProofsMessage,
        StdResult,
    };
    use slog_scope::{debug, warn};
    use std::{convert::Infallible, sync::Arc};
    use warp::http::StatusCode;

    use crate::{
        http_server::{routes::reply, validators::ProverTransactionsHashValidator},
        message_adapters::ToCardanoTransactionsProofsMessageAdapter,
        services::{ProverService, SignedEntityService},
        unwrap_to_internal_server_error,
    };

    use super::CardanoTransactionProofQueryParams;

    pub async fn proof_cardano_transaction(
        transaction_parameters: CardanoTransactionProofQueryParams,
        signed_entity_service: Arc<dyn SignedEntityService>,
        validator: ProverTransactionsHashValidator,
        prover_service: Arc<dyn ProverService>,
    ) -> Result<impl warp::Reply, Infallible> {
        let transaction_hashes = transaction_parameters.split_transactions_hashes();
        debug!(
            "⇄ HTTP SERVER: proof_cardano_transaction?transaction_hashes={}",
            transaction_parameters.transaction_hashes
        );

        if let Err(error) = validator.validate(&transaction_hashes) {
            warn!("proof_cardano_transaction::bad_request");
            return Ok(reply::bad_request(error.label, error.message));
        }

        let sanitized_hashes = transaction_parameters.sanitize();

        match unwrap_to_internal_server_error!(
            signed_entity_service
                .get_last_cardano_transaction_snapshot()
                .await,
            "proof_cardano_transaction::error"
        ) {
            Some(signed_entity) => {
                let message = unwrap_to_internal_server_error!(
                    build_response_message(prover_service, signed_entity, sanitized_hashes).await,
                    "proof_cardano_transaction"
                );
                Ok(reply::json(&message, StatusCode::OK))
            }
            None => {
                warn!("proof_cardano_transaction::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
        }
    }

    pub async fn build_response_message(
        prover_service: Arc<dyn ProverService>,
        signed_entity: SignedEntity<CardanoTransactionsSnapshot>,
        transaction_hashes: Vec<String>,
    ) -> StdResult<CardanoTransactionsProofsMessage> {
        let transactions_set_proofs = prover_service
            .compute_transactions_proofs(
                signed_entity.artifact.block_number,
                transaction_hashes.as_slice(),
            )
            .await?;
        let message = ToCardanoTransactionsProofsMessageAdapter::try_adapt(
            signed_entity,
            transactions_set_proofs,
            transaction_hashes,
        )?;

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use serde_json::Value::Null;
    use std::vec;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        entities::{CardanoTransactionsSetProof, CardanoTransactionsSnapshot, SignedEntity},
        test_utils::{apispec::APISpec, assert_equivalent, fake_data},
    };

    use crate::services::MockSignedEntityService;
    use crate::{
        dependency_injection::DependenciesBuilder, http_server::SERVER_BASE_PATH,
        services::MockProverService, Configuration,
    };

    use super::*;

    fn setup_router(
        dependency_manager: Arc<DependencyContainer>,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any()
            .and(warp::path(SERVER_BASE_PATH))
            .and(routes(dependency_manager).with(cors))
    }

    #[tokio::test]
    async fn build_response_message_return_latest_block_number_from_artifact_beacon() {
        // Arrange
        let mut mock_prover_service = MockProverService::new();
        mock_prover_service
            .expect_compute_transactions_proofs()
            .returning(|_, _| Ok(vec![CardanoTransactionsSetProof::dummy()]));

        let cardano_transactions_snapshot = CardanoTransactionsSnapshot::new(String::new(), 2309);

        let signed_entity = SignedEntity::<CardanoTransactionsSnapshot> {
            artifact: cardano_transactions_snapshot,
            ..SignedEntity::<CardanoTransactionsSnapshot>::dummy()
        };

        // Action
        let transaction_hashes = vec![];
        let message = handlers::build_response_message(
            Arc::new(mock_prover_service),
            signed_entity,
            transaction_hashes,
        )
        .await
        .unwrap();

        // Assert
        assert_eq!(message.latest_block_number, 2309)
    }

    #[tokio::test]
    async fn proof_cardano_transaction_ok() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let mut dependency_manager = builder.build_dependency_container().await.unwrap();
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_last_cardano_transaction_snapshot()
            .returning(|| Ok(Some(SignedEntity::<CardanoTransactionsSnapshot>::dummy())));
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let mut mock_prover_service = MockProverService::new();
        mock_prover_service
            .expect_compute_transactions_proofs()
            .returning(|_, _| Ok(vec![CardanoTransactionsSetProof::dummy()]));
        dependency_manager.prover_service = Arc::new(mock_prover_service);

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes={},{}",
                fake_data::transaction_hashes()[0],
                fake_data::transaction_hashes()[1]
            ))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn proof_cardano_transaction_not_found() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let dependency_manager = builder.build_dependency_container().await.unwrap();

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes={},{}",
                fake_data::transaction_hashes()[0],
                fake_data::transaction_hashes()[1]
            ))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn proof_cardano_transaction_ko() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let mut dependency_manager = builder.build_dependency_container().await.unwrap();
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_last_cardano_transaction_snapshot()
            .returning(|| Err(anyhow!("Error")));
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes={},{}",
                fake_data::transaction_hashes()[0],
                fake_data::transaction_hashes()[1]
            ))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn proof_cardano_transaction_return_bad_request_with_invalid_hashes() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let dependency_manager = builder.build_dependency_container().await.unwrap();

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes=invalid%3A%2F%2Fid,,tx-456"
            ))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::BAD_REQUEST,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn proof_cardano_transaction_route_deduplicate_hashes() {
        let tx = fake_data::transaction_hashes()[0].to_string();
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let mut dependency_manager = builder.build_dependency_container().await.unwrap();
        let mut mock_signed_entity_service = MockSignedEntityService::new();
        mock_signed_entity_service
            .expect_get_last_cardano_transaction_snapshot()
            .returning(|| Ok(Some(SignedEntity::<CardanoTransactionsSnapshot>::dummy())));
        dependency_manager.signed_entity_service = Arc::new(mock_signed_entity_service);

        let mut mock_prover_service = MockProverService::new();
        let txs_expected = vec![tx.clone()];
        mock_prover_service
            .expect_compute_transactions_proofs()
            .withf(move |_, transaction_hashes| transaction_hashes == txs_expected)
            .returning(|_, _| Ok(vec![CardanoTransactionsSetProof::dummy()]));
        dependency_manager.prover_service = Arc::new(mock_prover_service);

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes={tx},{tx}",
            ))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        assert_eq!(StatusCode::OK, response.status());
    }

    #[test]
    fn sanitize_cardano_transaction_proof_query_params_remove_duplicate() {
        let tx1 = fake_data::transaction_hashes()[0].to_string();
        let tx2 = fake_data::transaction_hashes()[1].to_string();

        // We are testing on an unordered list of transaction hashes
        // as some rust dedup methods only remove consecutive duplicates
        let params = CardanoTransactionProofQueryParams {
            transaction_hashes: format!("{tx1},{tx2},{tx2},{tx1},{tx2}",),
        };

        assert_equivalent(params.sanitize(), vec![tx1, tx2]);
    }
}
