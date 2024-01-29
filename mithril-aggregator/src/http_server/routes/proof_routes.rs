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
    pub fn split_transactions_hashes(&self) -> Vec<&str> {
        self.transaction_hashes.split(',').collect()
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
        .and(middlewares::with_prover_service(dependency_manager))
        .and_then(handlers::proof_cardano_transaction)
}

mod handlers {
    use std::{convert::Infallible, sync::Arc};

    use reqwest::StatusCode;
    use slog_scope::{debug, warn};

    use crate::{
        http_server::routes::reply, message_adapters::ToCardanoTransactionsProofsMessageAdapter,
        services::ProverService,
    };

    use super::CardanoTransactionProofQueryParams;

    pub async fn proof_cardano_transaction(
        transaction_parameters: CardanoTransactionProofQueryParams,
        prover_service: Arc<dyn ProverService>,
    ) -> Result<impl warp::Reply, Infallible> {
        let transaction_hashes = transaction_parameters
            .split_transactions_hashes()
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        debug!(
            "⇄ HTTP SERVER: proof_cardano_transaction?transaction_hashes={}",
            transaction_parameters.transaction_hashes
        );

        match prover_service
            .compute_transactions_proofs(transaction_hashes.as_slice())
            .await
        {
            Ok(transactions_set_proofs) => Ok(reply::json(
                &ToCardanoTransactionsProofsMessageAdapter::adapt(
                    transactions_set_proofs,
                    transaction_hashes,
                ),
                StatusCode::OK,
            )),
            Err(err) => {
                warn!("proof_cardano_transaction::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::test_utils::apispec::APISpec;

    use serde_json::Value::Null;
    use warp::{http::Method, test::request};

    use crate::{
        dependency_injection::DependenciesBuilder, http_server::SERVER_BASE_PATH, Configuration,
    };

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
    async fn proof_cardano_transaction_ok() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let dependency_manager = builder.build_dependency_container().await.unwrap();

        let method = Method::GET.as_str();
        let path = "/proof/cardano-transaction";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{path}?transaction_hashes=tx-123,tx-456"
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
        );
    }
}
