use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    epoch_settings(dependency_manager)
}

/// GET /epoch-settings
fn epoch_settings(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("epoch-settings")
        .and(warp::get())
        .and(middlewares::with_epoch_service(dependency_manager))
        .and_then(handlers::epoch_settings)
}

mod handlers {
    use crate::dependency_injection::EpochServiceWrapper;
    use crate::http_server::routes::reply;
    use crate::ToEpochSettingsMessageAdapter;
    use mithril_common::entities::EpochSettings;
    use mithril_common::messages::ToMessageAdapter;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use warp::http::StatusCode;

    /// Epoch Settings
    pub async fn epoch_settings(
        epoch_service: EpochServiceWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: epoch_settings");
        let epoch_service = epoch_service.read().await;

        match (
            epoch_service.epoch_of_current_data(),
            epoch_service.next_protocol_parameters(),
            epoch_service.upcoming_protocol_parameters(),
        ) {
            (Ok(epoch), Ok(protocol_parameters), Ok(next_protocol_parameters)) => {
                let epoch_settings = EpochSettings {
                    epoch,
                    protocol_parameters: protocol_parameters.clone(),
                    next_protocol_parameters: next_protocol_parameters.clone(),
                };
                let epoch_settings_message = ToEpochSettingsMessageAdapter::adapt(epoch_settings);
                Ok(reply::json(&epoch_settings_message, StatusCode::OK))
            }
            (Err(err), _, _) | (_, Err(err), _) | (_, _, Err(err)) => {
                warn!("epoch_settings::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::Epoch,
        test_utils::{apispec::APISpec, MithrilFixtureBuilder},
    };
    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::http::{Method, StatusCode};
    use warp::test::request;

    use crate::http_server::SERVER_BASE_PATH;
    use crate::initialize_dependencies;
    use crate::services::FakeEpochService;

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
    async fn test_epoch_settings_get_ok() -> Result<(), String> {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity_with_status(
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
    async fn test_epoch_settings_get_ko_500() -> Result<(), String> {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
        let dependency_manager = initialize_dependencies().await;

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity_with_status(
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
