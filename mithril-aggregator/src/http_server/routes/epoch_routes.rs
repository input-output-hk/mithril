use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    epoch_settings(dependency_manager)
}

/// GET /epoch-settings
fn epoch_settings(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("epoch-settings")
        .and(warp::get())
        .and(middlewares::with_protocol_parameters_store(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_multi_signer(dependency_manager))
        .and_then(handlers::epoch_settings)
}

mod handlers {
    use crate::dependency::MultiSignerWrapper;
    use crate::http_server::routes::reply;
    use crate::{ProtocolParametersStore, ProtocolParametersStorer, ToEpochSettingsMessageAdapter};
    use mithril_common::entities::EpochSettings;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    /// Epoch Settings
    pub async fn epoch_settings(
        protocol_parameters_store: Arc<ProtocolParametersStore>,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: epoch_settings");

        match multi_signer.read().await.get_current_beacon().await {
            Some(beacon) => {
                match (
                    protocol_parameters_store
                        .get_protocol_parameters(beacon.epoch)
                        .await,
                    protocol_parameters_store
                        .get_protocol_parameters(beacon.epoch.next())
                        .await,
                ) {
                    (Ok(Some(protocol_parameters)), Ok(Some(next_protocol_parameters))) => {
                        let epoch_settings = EpochSettings {
                            epoch: beacon.epoch,
                            protocol_parameters,
                            next_protocol_parameters,
                        };
                        let epoch_settings_message =
                            ToEpochSettingsMessageAdapter::adapt(epoch_settings);
                        Ok(reply::json(&epoch_settings_message, StatusCode::OK))
                    }
                    (Ok(None), Ok(Some(_))) | (Ok(Some(_)), Ok(None)) | (Ok(None), Ok(None)) => {
                        warn!("epoch_settings::could_not_retrieve_protocol_parameters");
                        Ok(reply::internal_server_error(
                            "could_not_retrieve_protocol_parameters".to_string(),
                        ))
                    }
                    (Err(err), _) | (_, Err(err)) => {
                        warn!("epoch_settings::error"; "error" => ?err);
                        Ok(reply::internal_server_error(err.to_string()))
                    }
                }
            }
            None => {
                warn!("epoch_settings::could_not_retrieve_epoch");
                Ok(reply::internal_server_error(
                    "could_not_retrieve_epoch".to_string(),
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::test_utils::apispec::APISpec;
    use serde_json::Value::Null;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::initialize_dependencies;

    fn setup_router(
        dependency_manager: Arc<DependencyManager>,
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
    async fn test_epoch_settings_get_ok() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
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
        );
    }

    #[tokio::test]
    async fn test_epoch_settings_get_ko_500() {
        let method = Method::GET.as_str();
        let path = "/epoch-settings";
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
        );
    }
}
