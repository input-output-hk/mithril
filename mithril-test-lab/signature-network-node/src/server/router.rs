use warp::{Filter, Reply};

/// Routes
pub fn routes(// dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    warp::any().and(pull_signatures())
}

/// GET /epoch-settings
fn pull_signatures() -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
    warp::path!("pull-signatures")
        .and(warp::get())
        .and_then(handlers::epoch_settings)
}

mod handlers {
    use std::convert::Infallible;
    use warp::hyper::StatusCode;

    use crate::server::reply;

    /// Epoch Settings
    pub async fn epoch_settings() -> Result<impl warp::Reply, Infallible> {
        Ok(reply::empty(StatusCode::NOT_IMPLEMENTED))
    }
}
