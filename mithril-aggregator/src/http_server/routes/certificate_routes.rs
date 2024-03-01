use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    certificate_pending(dependency_manager.clone())
        .or(certificate_certificates(dependency_manager.clone()))
        .or(certificate_certificate_hash(dependency_manager))
}

/// GET /certificate-pending
fn certificate_pending(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificate-pending")
        .and(warp::get())
        .and(middlewares::with_certificate_pending_store(
            dependency_manager,
        ))
        .and_then(handlers::certificate_pending)
}

/// GET /certificates
fn certificate_certificates(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificates")
        .and(warp::get())
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::certificate_certificates)
}

/// GET /certificate/{certificate_hash}
fn certificate_certificate_hash(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificate" / String)
        .and(warp::get())
        .and(middlewares::with_http_message_service(dependency_manager))
        .and_then(handlers::certificate_certificate_hash)
}

mod handlers {
    use crate::{
        http_server::routes::reply, services::MessageService, CertificatePendingStore,
        ToCertificatePendingMessageAdapter,
    };

    use mithril_common::messages::ToMessageAdapter;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// Certificate Pending
    pub async fn certificate_pending(
        certificate_pending_store: Arc<CertificatePendingStore>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: certificate_pending");

        match certificate_pending_store.get().await {
            Ok(Some(certificate_pending)) => Ok(reply::json(
                &ToCertificatePendingMessageAdapter::adapt(certificate_pending),
                StatusCode::OK,
            )),
            Ok(None) => Ok(reply::empty(StatusCode::NO_CONTENT)),
            Err(err) => {
                warn!("certificate_pending::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }

    /// List all Certificates
    pub async fn certificate_certificates(
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: certificate_certificates",);

        match http_message_service
            .get_certificate_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(certificates) => Ok(reply::json(&certificates, StatusCode::OK)),
            Err(err) => {
                warn!("certificate_certificates::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(
            "⇄ HTTP SERVER: certificate_certificate_hash/{}",
            certificate_hash
        );

        match http_message_service
            .get_certificate_message(&certificate_hash)
            .await
        {
            Ok(Some(certificate)) => Ok(reply::json(&certificate, StatusCode::OK)),
            Ok(None) => Ok(reply::empty(StatusCode::NOT_FOUND)),
            Err(err) => {
                warn!("certificate_certificate_hash::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use async_trait::async_trait;
    use mithril_common::{
        entities::CertificatePending,
        test_utils::{apispec::APISpec, fake_data},
    };
    use mithril_persistence::store::adapter::AdapterError;
    use mithril_persistence::store::adapter::StoreAdapter;
    use serde_json::Value::Null;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use crate::{
        http_server::SERVER_BASE_PATH, initialize_dependencies, services::MockMessageService,
        CertificatePendingStore,
    };

    use super::*;

    ///////////////

    pub struct MockStoreAdapter {}

    #[async_trait]
    impl StoreAdapter for MockStoreAdapter {
        type Key = String;
        type Record = CertificatePending;

        async fn store_record(
            &mut self,
            _key: &Self::Key,
            _record: &Self::Record,
        ) -> Result<(), AdapterError> {
            Ok(())
        }

        async fn get_record(&self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
            Err(AdapterError::GeneralError(anyhow!("Ca marche pô")))
        }

        async fn record_exists(&self, _key: &Self::Key) -> Result<bool, AdapterError> {
            Ok(true)
        }

        async fn get_last_n_records(
            &self,
            _how_many: usize,
        ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
            Ok(Vec::new())
        }

        async fn remove(&mut self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
            Ok(None)
        }

        async fn get_iter(
            &self,
        ) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
            Err(AdapterError::GeneralError(anyhow!("")))
        }
    }

    //////////////
    // use mockall::mock;

    // mock! {
    //     pub StoreAdapterTotoImpl<K: 'static, R: 'static> { }

    //     #[async_trait]
    //     impl<K, R> StoreAdapter for StoreAdapterTotoImpl<K, R> where K: Sync + Send , R: Sync + Send {
    //         type Key = K;
    //         type Record = R;

    //         /// Store the given `record`.
    //         async fn store_record(
    //             &mut self,
    //             key: &Self::Key,
    //             record: &Self::Record,
    //         ) -> Result<(), AdapterError>;

    //         /// Get the record stored using the given `key`.
    //         async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError>;

    //         /// Check if a record exist for the given `key`.
    //         async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError>;

    //         /// Get the last `n` records in the store
    //         async fn get_last_n_records(
    //             &self,
    //             how_many: usize,
    //         ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError>;

    //         /// remove values from store
    //         ///
    //         /// if the value exists it is returned by the adapter otherwise None is returned
    //         async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError>;

    //         /// Get an iterator over the stored values, from the latest to the oldest.
    //         async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError>;
    //     }
    // }

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
    async fn test_certificate_pending_with_content_get_ok_200() -> Result<(), String> {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let dependency_manager = initialize_dependencies().await;
        let certificate_pending = {
            let mut signers = fake_data::signers(3);
            signers[0].party_id = "1".to_string();
            CertificatePending {
                signers,
                ..fake_data::certificate_pending()
            }
        };
        dependency_manager
            .certificate_pending_store
            .save(certificate_pending)
            .await
            .unwrap();

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
    }

    #[tokio::test]
    async fn test_certificate_pending_without_content_get_ok_204() -> Result<(), String> {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let dependency_manager = initialize_dependencies().await;

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
            &StatusCode::NO_CONTENT,
        )
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ko_500() -> Result<(), String> {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let mut dependency_manager = initialize_dependencies().await;

        let adapter = MockStoreAdapter {};
        let certificate_pending_store_store = CertificatePendingStore::new(Box::new(adapter));
        dependency_manager.certificate_pending_store = Arc::new(certificate_pending_store_store);

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
    }

    #[tokio::test]
    async fn test_certificate_certificates_get_ok() -> Result<(), String> {
        let dependency_manager = initialize_dependencies().await;
        dependency_manager
            .certificate_repository
            .create_certificate(fake_data::genesis_certificate("{certificate_hash}"))
            .await
            .expect("certificate store save should have succeeded");

        let method = Method::GET.as_str();
        let path = "/certificates";

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
    }

    #[tokio::test]
    async fn test_certificate_when_error_retrieving_certificates_returns_ko_500(
    ) -> Result<(), String> {
        let mut dependency_manager = initialize_dependencies().await;
        let mut message_service = MockMessageService::new();
        message_service
            .expect_get_certificate_list_message()
            .returning(|_| Err(anyhow!("an error")));
        dependency_manager.message_service = Arc::new(message_service);

        let method = Method::GET.as_str();
        let path = "/certificates";

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
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok() -> Result<(), String> {
        let dependency_manager = initialize_dependencies().await;
        dependency_manager
            .certificate_repository
            .create_certificate(fake_data::genesis_certificate("{certificate_hash}"))
            .await
            .expect("certificate store save should have succeeded");

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        println!("Response: {:?}", response);

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok_404() -> Result<(), String> {
        let dependency_manager = initialize_dependencies().await;

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        println!("Response: {:?}", response);
        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
    }

    #[tokio::test]
    async fn test_certificate_when_error_on_retrieving_certificate_hash_returns_ko_500(
    ) -> Result<(), String> {
        let mut dependency_manager = initialize_dependencies().await;
        let mut message_service = MockMessageService::new();
        message_service
            .expect_get_certificate_message()
            .returning(|_| Err(anyhow!("an error")));
        dependency_manager.message_service = Arc::new(message_service);

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!(
                "/{SERVER_BASE_PATH}{}",
                path.replace("{certificate_hash}", "whatever")
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
    }
}
