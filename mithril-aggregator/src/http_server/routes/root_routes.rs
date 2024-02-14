use crate::DependencyContainer;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use warp::Filter;

use super::middlewares;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct RootRouteMessage {
    pub open_api_version: String,
    pub documentation_url: String,
}

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    root(dependency_manager)
}

/// GET /
fn root(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path::end()
        .and(middlewares::with_api_version_provider(dependency_manager))
        .and_then(handlers::root)
}

mod handlers {
    use mithril_common::api_version::APIVersionProvider;
    use reqwest::StatusCode;
    use slog_scope::{debug, warn};

    use crate::http_server::routes::{
        reply::{self, json},
        root_routes::RootRouteMessage,
    };
    use std::{convert::Infallible, sync::Arc};

    /// Root
    pub async fn root(
        api_version_provider: Arc<APIVersionProvider>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: root");

        match api_version_provider.compute_current_version() {
            Ok(open_api_version) => Ok(json(
                &RootRouteMessage {
                    open_api_version: open_api_version.to_string(),
                    documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
                },
                StatusCode::OK,
            )),
            Err(err) => {
                warn!("root::error"; "error" => ?err);
                Ok(reply::internal_server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::http_server::SERVER_BASE_PATH;
    use crate::{initialize_dependencies, DependencyContainer};
    use mithril_common::test_utils::apispec::APISpec;
    use reqwest::StatusCode;
    use serde_json::Value::Null;
    use std::sync::Arc;
    use warp::http::Method;
    use warp::test::request;
    use warp::Filter;

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
    async fn test_root_route_ok() {
        let method = Method::GET.as_str();
        let path = "/";
        let dependency_manager = initialize_dependencies().await;
        let expected_open_api_version = dependency_manager
            .api_version_provider
            .clone()
            .compute_current_version()
            .unwrap()
            .to_string();

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        let response_body: RootRouteMessage = serde_json::from_slice(response.body()).unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        assert_eq!(
            response_body,
            RootRouteMessage {
                open_api_version: expected_open_api_version,
                documentation_url: env!("CARGO_PKG_HOMEPAGE").to_string(),
            }
        );

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
