use warp::Filter;

use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    post_statistics(router_state)
        .or(post_cardano_database_immutable_files_restored(router_state))
        .or(post_cardano_database_ancillary_files_restored(router_state))
        .or(post_cardano_database_complete_restoration(router_state))
        .or(post_cardano_database_partial_restoration(router_state))
}

/// POST /statistics/snapshot
fn post_statistics(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "snapshot")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::post_snapshot_statistics)
}

/// POST /statistics/cardano-database/immutable-files-restored
fn post_cardano_database_immutable_files_restored(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "immutable-files-restored")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::post_cardano_database_immutable_files_restored)
}

/// POST /statistics/cardano-database/ancillary-files-restored
fn post_cardano_database_ancillary_files_restored(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "ancillary-files-restored")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::post_cardano_database_ancillary_files_restored)
}

/// POST /statistics/cardano-database/complete-restoration
fn post_cardano_database_complete_restoration(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "complete-restoration")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::post_cardano_database_complete_restoration)
}

/// POST /statistics/cardano-database/partial-restoration
fn post_cardano_database_partial_restoration(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("statistics" / "cardano-database" / "partial-restoration")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::post_cardano_database_partial_restoration)
}

mod handlers {
    use serde::Serialize;
    use slog::warn;
    use std::{convert::Infallible, sync::Arc};
    use warp::http::StatusCode;

    use mithril_common::messages::{
        CardanoDatabaseAncillaryFilesRestoredMessage, CardanoDatabaseCompleteRestorationMessage,
        CardanoDatabaseImmutableFilesRestoredMessage, CardanoDatabasePartialRestorationMessage,
        SnapshotDownloadMessage,
    };

    use crate::event_store::{EventMessage, TransmitterService};
    use crate::http_server::routes::reply;
    use crate::MetricsService;

    pub async fn post_snapshot_statistics(
        snapshot_download_message: SnapshotDownloadMessage,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_cardano_immutable_files_full_total_restoration_since_startup()
            .increment();

        let headers: Vec<(&str, &str)> = Vec::new();

        let message = EventMessage::new(
            "HTTP::statistics",
            "snapshot_downloaded",
            &snapshot_download_message,
            headers,
        );

        match event_transmitter.try_send(message.clone()) {
            Err(e) => {
                warn!(logger, "Event message error"; "error" => ?e);
                Ok(reply::internal_server_error(e))
            }
            Ok(_) => Ok(reply::empty(StatusCode::CREATED)),
        }
    }

    pub async fn post_cardano_database_immutable_files_restored(
        message: CardanoDatabaseImmutableFilesRestoredMessage,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_cardano_database_immutable_files_restored()
            .increment_by(message.nb_immutable_files);

        transmit_event_message(
            message,
            logger,
            event_transmitter,
            "cardano_database_immutable_files_restored",
        )
        .await
    }

    pub async fn post_cardano_database_ancillary_files_restored(
        message: CardanoDatabaseAncillaryFilesRestoredMessage,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_cardano_database_ancillary_files_restored()
            .increment_by(message.nb_ancillary_files);

        transmit_event_message(
            message,
            logger,
            event_transmitter,
            "cardano_database_ancillary_files_restored",
        )
        .await
    }

    pub async fn post_cardano_database_complete_restoration(
        message: CardanoDatabaseCompleteRestorationMessage,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_cardano_database_complete_restoration()
            .increment_by(message.nb_complete_restoration);

        transmit_event_message(
            message,
            logger,
            event_transmitter,
            "cardano_database_complete_restoration",
        )
        .await
    }

    pub async fn post_cardano_database_partial_restoration(
        message: CardanoDatabasePartialRestorationMessage,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        metrics_service
            .get_cardano_database_partial_restoration()
            .increment_by(message.nb_partial_restoration);

        transmit_event_message(
            message,
            logger,
            event_transmitter,
            "cardano_database_partial_restoration",
        )
        .await
    }

    async fn transmit_event_message<T: Serialize>(
        message: T,
        logger: slog::Logger,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        action: &str,
    ) -> Result<impl warp::Reply, Infallible> {
        let headers: Vec<(&str, &str)> = Vec::new();

        let message = EventMessage::new("HTTP::statistics", action, &message, headers);

        match event_transmitter.try_send(message.clone()) {
            Err(e) => {
                warn!(logger, "Event message error"; "error" => ?e);
                Ok(reply::internal_server_error(e))
            }
            Ok(_) => Ok(reply::empty(StatusCode::CREATED)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::messages::{
        CardanoDatabaseImmutableFilesRestoredMessage, SnapshotDownloadMessage,
    };
    use mithril_common::test_utils::apispec::APISpec;

    use std::sync::Arc;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use crate::{
        dependency_injection::DependenciesBuilder, initialize_dependencies, Configuration,
    };

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
    async fn post_statistics_ok() {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new_with_stdout_logger(config);
        let mut rx = builder.get_event_transmitter_receiver().await.unwrap();
        let dependency_manager = builder.build_dependency_container().await.unwrap();
        let snapshot_download_message = SnapshotDownloadMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/statistics/snapshot";

        let response = request()
            .method(method)
            .json(&snapshot_download_message)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        let result = APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &snapshot_download_message,
            &response,
            &StatusCode::CREATED,
        );

        let _ = rx.try_recv().unwrap();
        result.unwrap();
    }

    #[tokio::test]
    async fn test_post_statistics_increments_cardano_db_total_restoration_since_startup_metric() {
        let method = Method::POST.as_str();
        let path = "/statistics/snapshot";
        let dependency_manager = Arc::new(initialize_dependencies().await);
        let initial_counter_value = dependency_manager
            .metrics_service
            .get_cardano_immutable_files_full_total_restoration_since_startup()
            .get();

        request()
            .method(method)
            .json(&SnapshotDownloadMessage::dummy())
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(
                dependency_manager.clone(),
            )))
            .await;

        assert_eq!(
            initial_counter_value + 1,
            dependency_manager
                .metrics_service
                .get_cardano_immutable_files_full_total_restoration_since_startup()
                .get()
        );
    }

    mod restoration {
        use mithril_metric::CounterValue;
        use serde::Serialize;
        use tokio::sync::mpsc::UnboundedReceiver;

        use crate::{event_store::EventMessage, DependencyContainer, MetricsService};

        use super::*;

        async fn setup_dependencies() -> (Arc<DependencyContainer>, UnboundedReceiver<EventMessage>)
        {
            let config = Configuration::new_sample();
            let mut builder = DependenciesBuilder::new_with_stdout_logger(config);
            let rx = builder.get_event_transmitter_receiver().await.unwrap();
            let dependency_manager = Arc::new(builder.build_dependency_container().await.unwrap());
            (dependency_manager, rx)
        }

        async fn check_conform_to_open_api_when_created(
            method: Method,
            path: &str,
            message: &impl Serialize,
        ) {
            let (dependency_manager, _rx) = setup_dependencies().await;

            let response = request()
                .method(method.as_str())
                .json(message)
                .path(path)
                .reply(&setup_router(RouterState::new_with_dummy_config(
                    dependency_manager.clone(),
                )))
                .await;

            let result = APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                method.as_str(),
                path,
                "application/json",
                message,
                &response,
                &StatusCode::CREATED,
            );

            result.unwrap();
        }

        async fn check_conform_to_open_api_when_server_error(
            method: Method,
            path: &str,
            message: &impl Serialize,
        ) {
            let (dependency_manager, mut rx) = setup_dependencies().await;
            rx.close();

            let response = request()
                .method(method.as_str())
                .json(message)
                .path(path)
                .reply(&setup_router(RouterState::new_with_dummy_config(
                    dependency_manager.clone(),
                )))
                .await;

            APISpec::verify_conformity(
                APISpec::get_all_spec_files(),
                method.as_str(),
                path,
                "application/json",
                &message,
                &response,
                &StatusCode::INTERNAL_SERVER_ERROR,
            )
            .unwrap();
        }

        async fn check_should_send_event(method: Method, path: &str, message: &impl Serialize) {
            let (dependency_manager, mut rx) = setup_dependencies().await;

            request()
                .method(method.as_str())
                .json(message)
                .path(path)
                .reply(&setup_router(RouterState::new_with_dummy_config(
                    dependency_manager.clone(),
                )))
                .await;

            let _ = rx.try_recv().unwrap();
        }

        async fn check_increments_metric(
            method: Method,
            path: &str,
            message: &impl Serialize,
            expected_increment: u32,
            get_metric_value: &dyn Fn(Arc<MetricsService>) -> CounterValue,
        ) {
            let (dependency_manager, _rx) = setup_dependencies().await;

            let initial_counter_value =
                get_metric_value(dependency_manager.metrics_service.clone());

            request()
                .method(method.as_str())
                .json(message)
                .path(path)
                .reply(&setup_router(RouterState::new_with_dummy_config(
                    dependency_manager.clone(),
                )))
                .await;

            assert_eq!(
                initial_counter_value + expected_increment,
                get_metric_value(dependency_manager.metrics_service.clone())
            );
        }

        mod post_cardano_database_immutable_files_restored {
            use super::*;

            const HTTP_METHOD: Method = Method::POST;
            const PATH: &str = "/statistics/cardano-database/immutable-files-restored";

            #[tokio::test]
            async fn conform_to_open_api_when_created() {
                let message = CardanoDatabaseImmutableFilesRestoredMessage::dummy();

                check_conform_to_open_api_when_created(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn conform_to_open_api_when_server_error() {
                let message = CardanoDatabaseImmutableFilesRestoredMessage::dummy();

                check_conform_to_open_api_when_server_error(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn should_send_event() {
                let message = CardanoDatabaseImmutableFilesRestoredMessage::dummy();

                check_should_send_event(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn increments_metric() {
                let message = CardanoDatabaseImmutableFilesRestoredMessage {
                    nb_immutable_files: 3,
                };

                check_increments_metric(
                    HTTP_METHOD,
                    PATH,
                    &message,
                    message.nb_immutable_files,
                    &|metrics_service: Arc<MetricsService>| {
                        metrics_service
                            .get_cardano_database_immutable_files_restored()
                            .get()
                    },
                )
                .await;
            }
        }

        mod post_cardano_database_ancillary_files_restored {
            use mithril_common::messages::CardanoDatabaseAncillaryFilesRestoredMessage;

            use super::*;

            const HTTP_METHOD: Method = Method::POST;
            const PATH: &str = "/statistics/cardano-database/ancillary-files-restored";

            #[tokio::test]
            async fn conform_to_open_api_when_created() {
                let message = CardanoDatabaseAncillaryFilesRestoredMessage::dummy();

                check_conform_to_open_api_when_created(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn conform_to_open_api_when_server_error() {
                let message = CardanoDatabaseAncillaryFilesRestoredMessage::dummy();

                check_conform_to_open_api_when_server_error(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn should_send_event() {
                let message = CardanoDatabaseAncillaryFilesRestoredMessage::dummy();

                check_should_send_event(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn increments_metric() {
                let message = CardanoDatabaseAncillaryFilesRestoredMessage {
                    nb_ancillary_files: 3,
                };

                check_increments_metric(
                    HTTP_METHOD,
                    PATH,
                    &message,
                    message.nb_ancillary_files,
                    &|metrics_service: Arc<MetricsService>| {
                        metrics_service
                            .get_cardano_database_ancillary_files_restored()
                            .get()
                    },
                )
                .await;
            }
        }

        mod post_cardano_database_complete_restoration {
            use mithril_common::messages::CardanoDatabaseCompleteRestorationMessage;

            use super::*;

            const HTTP_METHOD: Method = Method::POST;
            const PATH: &str = "/statistics/cardano-database/complete-restoration";

            #[tokio::test]
            async fn conform_to_open_api_when_created() {
                let message = CardanoDatabaseCompleteRestorationMessage::dummy();

                check_conform_to_open_api_when_created(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn conform_to_open_api_when_server_error() {
                let message = CardanoDatabaseCompleteRestorationMessage::dummy();

                check_conform_to_open_api_when_server_error(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn should_send_event() {
                let message = CardanoDatabaseCompleteRestorationMessage::dummy();

                check_should_send_event(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn increments_metric() {
                let message = CardanoDatabaseCompleteRestorationMessage {
                    nb_complete_restoration: 3,
                };

                check_increments_metric(
                    HTTP_METHOD,
                    PATH,
                    &message,
                    message.nb_complete_restoration,
                    &|metrics_service: Arc<MetricsService>| {
                        metrics_service
                            .get_cardano_database_complete_restoration()
                            .get()
                    },
                )
                .await;
            }
        }

        mod post_cardano_database_partial_restoration {
            use mithril_common::messages::CardanoDatabasePartialRestorationMessage;

            use super::*;

            const HTTP_METHOD: Method = Method::POST;
            const PATH: &str = "/statistics/cardano-database/partial-restoration";

            #[tokio::test]
            async fn conform_to_open_api_when_created() {
                let message = CardanoDatabasePartialRestorationMessage::dummy();

                check_conform_to_open_api_when_created(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn conform_to_open_api_when_server_error() {
                let message = CardanoDatabasePartialRestorationMessage::dummy();

                check_conform_to_open_api_when_server_error(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn should_send_event() {
                let message = CardanoDatabasePartialRestorationMessage::dummy();

                check_should_send_event(HTTP_METHOD, PATH, &message).await;
            }

            #[tokio::test]
            async fn increments_metric() {
                let message = CardanoDatabasePartialRestorationMessage {
                    nb_partial_restoration: 3,
                };

                check_increments_metric(
                    HTTP_METHOD,
                    PATH,
                    &message,
                    message.nb_partial_restoration,
                    &|metrics_service: Arc<MetricsService>| {
                        metrics_service
                            .get_cardano_database_partial_restoration()
                            .get()
                    },
                )
                .await;
            }
        }
    }
}
