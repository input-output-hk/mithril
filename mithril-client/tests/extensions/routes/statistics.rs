use axum::response::IntoResponse;
use axum::routing::post;
use axum::Router;

pub fn routes() -> Router {
    Router::new()
        .route("/statistics/snapshot", post(created))
        .route(
            "/statistics/cardano-database/immutable-files-restored",
            post(created),
        )
        .route(
            "/statistics/cardano-database/ancillary-files-restored",
            post(created),
        )
        .route(
            "/statistics/cardano-database/partial-restoration",
            post(created),
        )
}

async fn created() -> impl IntoResponse {
    http::status::StatusCode::CREATED
}
