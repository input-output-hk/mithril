use warp::Filter;

use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    certificate_certificates(router_state).or(certificate_certificate_hash(router_state))
}

/// GET /certificates
fn certificate_certificates(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificates")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_http_message_service(router_state))
        .and_then(handlers::certificate_certificates)
}

/// GET /certificate/{certificate_hash}
fn certificate_certificate_hash(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("certificate" / String)
        .and(warp::get())
        .and(middlewares::with_origin_tag(router_state))
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_http_message_service(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::certificate_certificate_hash)
}

mod handlers {
    use crate::MetricsService;
    use crate::{http_server::routes::reply, services::MessageService};

    use slog::{warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    pub const LIST_MAX_ITEMS: usize = 20;

    /// List all Certificates
    pub async fn certificate_certificates(
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
    ) -> Result<impl warp::Reply, Infallible> {
        match http_message_service
            .get_certificate_list_message(LIST_MAX_ITEMS)
            .await
        {
            Ok(certificates) => Ok(reply::json(&certificates, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"certificate_certificates::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
        origin_tag: Option<String>,
        logger: Logger,
        http_message_service: Arc<dyn MessageService>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_certificate_detail_total_served_since_startup()
            .increment(&[origin_tag.unwrap_or_default().as_str()]);

        match http_message_service
            .get_certificate_message(&certificate_hash)
            .await
        {
            Ok(Some(certificate)) => Ok(reply::json(&certificate, StatusCode::OK)),
            Ok(None) => Ok(reply::empty(StatusCode::NOT_FOUND)),
            Err(err) => {
                warn!(logger,"certificate_certificate_hash::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::{
        test_utils::{apispec::APISpec, fake_data},
        MITHRIL_ORIGIN_TAG_HEADER,
    };
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use crate::{initialize_dependencies, services::MockMessageService};

    use super::*;

    fn setup_router(
        state: RouterState,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(routes(&state).with(cors))
    }

    #[tokio::test]
    async fn test_certificate_certificates_get_ok() {
        let dependency_manager = initialize_dependencies!().await;
        dependency_manager
            .certificate_repository
            .create_certificate(fake_data::genesis_certificate("{certificate_hash}"))
            .await
            .expect("certificate store save should have succeeded");

        let method = Method::GET.as_str();
        let path = "/certificates";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
    async fn test_certificate_when_error_retrieving_certificates_returns_ko_500() {
        let mut dependency_manager = initialize_dependencies!().await;
        let mut message_service = MockMessageService::new();
        message_service
            .expect_get_certificate_list_message()
            .returning(|_| Err(anyhow!("an error")));
        dependency_manager.message_service = Arc::new(message_service);

        let method = Method::GET.as_str();
        let path = "/certificates";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
    async fn test_certificate_certificate_hash_increments_certificate_detail_total_served_since_startup_metric(
    ) {
        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let dependency_manager = Arc::new(initialize_dependencies!().await);
        let metric = dependency_manager
            .metrics_service
            .get_certificate_detail_total_served_since_startup();
        let initial_counter_value_with_tag = metric.get(&["TEST"]);
        let initial_counter_value_with_unknown_tag = metric.get(&["UNKNOWN"]);

        request()
            .method(method)
            .path(path)
            .header(MITHRIL_ORIGIN_TAG_HEADER, "TEST")
            .reply(&setup_router(RouterState::new_with_origin_tag_white_list(
                dependency_manager.clone(),
                &["TEST"],
            )))
            .await;

        assert_eq!(initial_counter_value_with_tag + 1, metric.get(&["TEST"]));
        assert_eq!(
            initial_counter_value_with_unknown_tag,
            metric.get(&["UNKNOWN"])
        );
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok() {
        let dependency_manager = initialize_dependencies!().await;
        dependency_manager
            .certificate_repository
            .create_certificate(fake_data::genesis_certificate("{certificate_hash}"))
            .await
            .expect("certificate store save should have succeeded");

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
    async fn test_certificate_certificate_hash_get_ok_404() {
        let dependency_manager = initialize_dependencies!().await;

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
    async fn test_certificate_when_error_on_retrieving_certificate_hash_returns_ko_500() {
        let mut dependency_manager = initialize_dependencies!().await;
        let mut message_service = MockMessageService::new();
        message_service
            .expect_get_certificate_message()
            .returning(|_| Err(anyhow!("an error")));
        dependency_manager.message_service = Arc::new(message_service);

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&path.replace("{certificate_hash}", "whatever"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
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
