use warp::Filter;

use mithril_common::{messages::AggregatorStatusMessage, StdResult};

use crate::{
    dependency_injection::EpochServiceWrapper,
    http_server::routes::{middlewares, router::RouterState},
};

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    status(router_state)
}

/// GET /status
fn status(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("status")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_epoch_service(router_state))
        .and_then(handlers::status)
}

async fn get_aggregator_status_message(
    epoch_service: EpochServiceWrapper,
) -> StdResult<AggregatorStatusMessage> {
    let epoch_service = epoch_service.read().await;

    let epoch = epoch_service.epoch_of_current_data()?;
    let cardano_era = epoch_service.cardano_era()?;
    let mithril_era = epoch_service.mithril_era()?;

    let message = AggregatorStatusMessage {
        epoch,
        cardano_era,
        mithril_era,
    };

    Ok(message)
}

mod handlers {
    use std::convert::Infallible;

    use slog::{warn, Logger};
    use warp::http::StatusCode;

    use crate::{
        dependency_injection::EpochServiceWrapper,
        http_server::routes::{aggregator_status::get_aggregator_status_message, reply},
    };

    /// Status
    pub async fn status(
        logger: Logger,
        epoch_service: EpochServiceWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        let aggregator_status_message = get_aggregator_status_message(epoch_service).await;

        match aggregator_status_message {
            Ok(message) => Ok(reply::json(&message, StatusCode::OK)),
            Err(err) => {
                warn!(logger,"aggregator_status::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_common::{
        entities::Epoch, test_utils::apispec::APISpec, test_utils::MithrilFixtureBuilder,
    };

    use crate::{
        http_server::SERVER_BASE_PATH, initialize_dependencies, services::FakeEpochService,
    };

    use super::*;

    fn setup_router(
        state: RouterState,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any()
            .and(warp::path(SERVER_BASE_PATH))
            .and(routes(&state).with(cors))
    }

    #[tokio::test]
    async fn status_route_ko_500() {
        let dependency_manager = initialize_dependencies().await;
        let method = Method::GET.as_str();
        let path = "/status";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
    async fn status_route_ok_200() {
        let mut dependency_manager = initialize_dependencies().await;
        let fixture = MithrilFixtureBuilder::default().build();
        let epoch_service = FakeEpochService::from_fixture(Epoch(5), &fixture);
        dependency_manager.epoch_service = Arc::new(RwLock::new(epoch_service));

        let method = Method::GET.as_str();
        let path = "/status";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
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
}
