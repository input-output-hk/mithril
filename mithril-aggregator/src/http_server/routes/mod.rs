mod artifact_routes;
mod certificate_routes;
mod epoch_routes;
mod middlewares;
mod proof_routes;
pub(crate) mod reply;
mod root_routes;
pub mod router;
mod signatures_routes;
mod signer_routes;
mod statistics_routes;

/// Match the given result and do an early return with an internal server error (500)
/// if it was an Error. Else return the unwrapped value.
#[macro_export]
macro_rules! unwrap_to_internal_server_error {
    ($code:expr, $($warn_comment:tt)*) => {
        match $code {
            Ok(res) => res,
            Err(err) => {
                warn!($($warn_comment)*; "error" => ?err);
                return Ok($crate::http_server::routes::reply::internal_server_error(
                    err,
                ));
            }
        }
    };
}
