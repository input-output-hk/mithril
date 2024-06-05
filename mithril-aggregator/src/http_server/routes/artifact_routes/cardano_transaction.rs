use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    artifact_cardano_transactions(dependency_manager.clone())
        .or(artifact_cardano_transaction_by_id(dependency_manager))
}

/// GET /artifact/cardano-transactions
fn artifact_cardano_transactions(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-transactions")
        .and(warp::get())
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::list_artifacts)
}

/// GET /artifact/cardano-transaction/:id
fn artifact_cardano_transaction_by_id(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("artifact" / "cardano-transaction" / String)
        .and(warp::get())
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::get_artifact_by_signed_entity_id)
}

pub mod handlers {
    use crate::http_server::routes::reply;
    use crate::services::MessageService;

    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List Cardano Transactions set artifacts
    pub async fn list_artifacts(
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifacts");

        match http_message_service
            .get_cardano_transaction_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!("list_artifacts_cardano_transactions"; "error" => ?err);

                Ok(reply::internal_server_error(err))
            }
        }
    }

    /// Get Artifact by signed entity id
    pub async fn get_artifact_by_signed_entity_id(
        signed_entity_id: String,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: artifact/{signed_entity_id}");

        match http_message_service
            .get_cardano_transaction_message(&signed_entity_id)
            .await
        {
            Ok(Some(message)) => Ok(reply::json(&message, StatusCode::OK)),
            Ok(None) => {
                warn!("get_cardano_transaction_details::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!("get_cardano_transaction_details::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::http_server::routes::artifact_routes::test_utils::*;
    use crate::{
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        message_adapters::{
            ToCardanoTransactionListMessageAdapter, ToCardanoTransactionMessageAdapter,
        },
        services::MockMessageService,
    };
    use mithril_common::{
        entities::{Epoch, SignedEntityType},
        messages::ToMessageAdapter,
        test_utils::{apispec::APISpec, fake_data},
    };
    use mithril_persistence::sqlite::HydrationError;
    use serde_json::Value::Null;
    use warp::{
        http::{Method, StatusCode},
        test::request,
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
    async fn test_cardano_transactions_get_ok() {
        let signed_entity_records = create_signed_entities(
            SignedEntityType::CardanoTransactions(Epoch(1), 120),
            fake_data::cardano_transactions_snapshot(5),
        );
        let message = ToCardanoTransactionListMessageAdapter::adapt(signed_entity_records);
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_transaction_list_message()
            .return_once(|_| Ok(message))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-transactions";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
    async fn test_cardano_transactions_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_transaction_list_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-transactions";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
    async fn test_cardano_transaction_get_ok() {
        let signed_entity = create_signed_entities(
            SignedEntityType::CardanoTransactions(Epoch(1), 100),
            fake_data::cardano_transactions_snapshot(1),
        )
        .first()
        .unwrap()
        .to_owned();
        let message = ToCardanoTransactionMessageAdapter::adapt(signed_entity);
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_transaction_message()
            .return_once(|_| Ok(Some(message)))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-transaction/{hash}";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
    async fn test_cardano_transaction_return_404_not_found_when_no_record() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_transaction_message()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-transaction/{hash}";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
    async fn test_cardano_transaction_get_ko() {
        let mut mock_http_message_service = MockMessageService::new();
        mock_http_message_service
            .expect_get_cardano_transaction_message()
            .return_once(|_| Err(HydrationError::InvalidData("invalid data".to_string()).into()))
            .once();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.message_service = Arc::new(mock_http_message_service);

        let method = Method::GET.as_str();
        let path = "/artifact/cardano-transaction/{hash}";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
}
